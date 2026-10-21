/*
A re-implementation of the tactic state display from the Lean 4 infoview
(https://github.com/leanprover/vscode-lean4, `lean4-infoview/src/infoview/{goals,info}.tsx`)
using only the public API of `@leanprover/infoview`.

The component hierarchy mirrors the original:

  GoalInfoDisplay        (`info.tsx`; here the `default` export, usable as a panel widget)
  └─ FilteredGoals       (`goals.tsx`)
     └─ Details          (`collapsing.tsx`)
        └─ Goals
           └─ Goal
              ├─ Hyp
              │  ├─ HypName
              │  └─ InteractiveCode  (from `@leanprover/infoview`)
              └─ InteractiveCode

Deviations from the original are marked with `NOTE(deviation)`.
*/

import * as React from 'react'
import {
    DocumentPosition,
    EditorContext,
    FVarId,
    GoalsLocation,
    InfoviewActionKind,
    InfoviewConfig,
    InteractiveCode,
    InteractiveGoal,
    InteractiveGoals,
    InteractiveHypothesisBundle,
    InteractiveHypothesisBundle_nonAnonymousNames,
    LocationsContext,
    MVarId,
    PanelWidgetProps,
    TaggedText_stripTags,
    defaultInfoviewConfig,
    useEvent,
    useEventResult,
} from '@leanprover/infoview'

/** The type of the (unexported) `Locations` interface from `goalLocation.tsx`. */
type Locations = NonNullable<React.ContextType<typeof LocationsContext>>

/* ---------------------------------------------------------------------------------------------
 * Copies of helpers that are not exported from `@leanprover/infoview`.
 * They depend only on React, the DOM, and exported infoview APIs.
 * ------------------------------------------------------------------------------------------- */

/** Copy of the unexported `ConfigContext`.
 * The infoview computes it in `main.tsx` exactly like this. */
function useInfoviewConfig(): InfoviewConfig {
    const ec = React.useContext(EditorContext)
    return useEventResult(ec.events.changedInfoviewConfig) ?? defaultInfoviewConfig
}

/** Copy of `isAnyTextSelected` from `util.ts`. */
function isAnyTextSelected(): boolean {
    const s = window.getSelection()
    return s !== null && !s.isCollapsed
}

/** Copy of `preventClickOnTextSelection` from `util.ts`. */
function preventClickOnTextSelection(e: React.MouseEvent<HTMLElement, MouseEvent>) {
    if (isAnyTextSelected()) {
        e.preventDefault()
        return true
    }
    return false
}

/** Copy of `withPreventedClickOnTextSelection` from `util.ts`. */
function withPreventedClickOnTextSelection(
    f: React.MouseEventHandler<HTMLElement>,
): React.MouseEventHandler<HTMLElement> {
    return e => {
        const isPrevented = preventClickOnTextSelection(e)
        if (isPrevented) {
            return
        }
        f(e)
    }
}

/** Copy of `preventDoubleClickTextSelection` from `util.ts`. */
function preventDoubleClickTextSelection(e: React.MouseEvent<HTMLElement, MouseEvent>): boolean {
    if (e.detail > 1) {
        e.preventDefault()
        return true
    }
    return false
}

interface DetailsProps extends React.PropsWithoutRef<React.HTMLProps<HTMLDetailsElement>> {
    initiallyOpen?: boolean
    children: [React.ReactNode, ...React.ReactNode[]]
    setOpenRef?: (_: React.Dispatch<React.SetStateAction<boolean>>) => void
    selectable?: boolean | undefined
}

/** Copy of `Details` from `collapsing.tsx`.
 *
 * Like `<details>` but can be programatically revealed using `setOpenRef`.
 * The first child is placed inside the `<summary>` node. */
function Details({
    initiallyOpen,
    children: [summary, ...children],
    setOpenRef,
    selectable,
    ...props
}: DetailsProps): JSX.Element {
    const [isOpen, setOpen] = React.useState<boolean>(initiallyOpen === undefined ? false : initiallyOpen)
    if (setOpenRef) setOpenRef(setOpen)
    return (
        <details open={isOpen} {...props}>
            <summary
                className={'mv2 pointer ' + (!selectable ? 'non-selectable ' : '')}
                onClick={withPreventedClickOnTextSelection(e => {
                    if (e.defaultPrevented) {
                        e.preventDefault()
                        return
                    }
                    setOpen(!isOpen)
                    // See https://github.com/facebook/react/issues/15486#issuecomment-873516817
                    e.preventDefault()
                })}
                onMouseDown={e => preventDoubleClickTextSelection(e)}
            >
                {summary}
            </summary>
            {isOpen && children}
        </details>
    )
}

/** Copy of `HoverState` from `tooltips.tsx`. */
type HoverState = 'off' | 'over' | 'ctrlOver'

interface HoverHighlightSettings {
    ref: React.RefObject<HTMLSpanElement>
    /** Whether to return the `highlight` CSS class on hover. */
    highlightOnHover: boolean
    /** Whether to return the `underline` CSS class on hover while holding `Ctrl` / `Meta`. */
    underlineOnModHover: boolean
}

interface HoverHighlight {
    hoverState: HoverState
    setHoverState: React.Dispatch<React.SetStateAction<HoverState>>
    className: string
    onPointerOver: (e: React.PointerEvent<HTMLSpanElement>) => void
    onPointerOut: (e: React.PointerEvent<HTMLSpanElement>) => void
    onPointerMove: (e: React.PointerEvent<HTMLSpanElement>) => void
}

/** Copy of `useHoverHighlight` from `hoverHighlight.tsx`.
 *
 * Logic for a component that is highlighted/underlined when hovered over.
 * The component is passed in `settings.ref`. */
function useHoverHighlight(settings: HoverHighlightSettings): HoverHighlight {
    const { ref, highlightOnHover, underlineOnModHover } = settings

    const [hoverState, setHoverState] = React.useState<HoverState>('off')
    const isHoveredOver = hoverState !== 'off'

    let className: string = ''
    if (highlightOnHover && isHoveredOver) {
        className += 'highlight '
    }
    if (underlineOnModHover && hoverState === 'ctrlOver') {
        className += 'underline '
    }

    const onPointerEvent = (b: boolean, e: React.PointerEvent<HTMLSpanElement>) => {
        // We only want to handle hovers in the innermost component,
        // so we record that the event was handled with a property.
        // The same property name as in the infoview is used
        // so that nested infoview components (e.g. `InteractiveCode`) cooperate.
        if (ref.current && e.target instanceof Node && ref.current.contains(e.target)) {
            if ('_DetectHoverSpanSeen' in e) {
                return
            }
            ;(e as any)._DetectHoverSpanSeen = {}
            if (!b) {
                setHoverState('off')
            } else if (e.ctrlKey || e.metaKey) {
                setHoverState('ctrlOver')
            } else {
                setHoverState('over')
            }
        }
    }
    const onPointerOver = (e: React.PointerEvent<HTMLSpanElement>) => onPointerEvent(true, e)
    const onPointerOut = (e: React.PointerEvent<HTMLSpanElement>) => onPointerEvent(false, e)

    const onPointerMove = (e: React.PointerEvent<HTMLSpanElement>) => {
        if (e.ctrlKey || e.metaKey) {
            setHoverState(st => (st === 'over' ? 'ctrlOver' : st))
        } else {
            setHoverState(st => (st === 'ctrlOver' ? 'over' : st))
        }
    }

    const onKeyDown = React.useCallback((e: KeyboardEvent) => {
        if (e.key === 'Control' || e.key === 'Meta') {
            setHoverState(st => (st === 'over' ? 'ctrlOver' : st))
        }
    }, [])

    const onKeyUp = React.useCallback((e: KeyboardEvent) => {
        if (e.key === 'Control' || e.key === 'Meta') {
            setHoverState(st => (st === 'ctrlOver' ? 'over' : st))
        }
    }, [])

    React.useEffect(() => {
        if (!isHoveredOver) {
            // Avoid adding lots of expensive global event handlers for spans that are not being
            // hovered over
            return
        }

        // These event handlers do not fire when the InfoView is not focused.
        document.addEventListener('keydown', onKeyDown)
        document.addEventListener('keyup', onKeyUp)

        return () => {
            document.removeEventListener('keydown', onKeyDown)
            document.removeEventListener('keyup', onKeyUp)
        }
    }, [onKeyDown, onKeyUp, isHoveredOver])

    return {
        hoverState,
        setHoverState,
        className,
        onPointerOver,
        onPointerOut,
        onPointerMove,
    }
}

type SelectableLocationSettings =
    | {
          isSelectable: true
          /** The {@link GoalsLocation} that can be selected by interacting with the span. */
          loc: GoalsLocation
      }
    | { isSelectable: false }

interface SelectableLocation {
    className: string
    /** Returns whether propagation of the click event within the same handler should be stopped. */
    onClick: (e: React.MouseEvent<HTMLSpanElement, MouseEvent>) => boolean
    onPointerDown: (e: React.PointerEvent<HTMLSpanElement>) => void
    /** An object that should be spliced into `data-vscode-context`. */
    dataVscodeContext: object
}

/** Copy of `useSelectableLocation` from `goalLocation.tsx`.
 *
 * Logic for a component that can be selected using Shift-click and is highlighted when selected. */
function useSelectableLocation(settings: SelectableLocationSettings): SelectableLocation {
    const locs = React.useContext(LocationsContext)
    const ec = React.useContext(EditorContext)

    let className: string = ''
    if (settings.isSelectable && locs && locs.isSelected(settings.loc)) {
        className += 'highlight-selected '
    }

    const onClick = (e: React.MouseEvent<HTMLSpanElement, MouseEvent>) => {
        // On shift-click, if we are in a context where selecting subexpressions makes sense,
        // (un)select the current subexpression.
        if (settings.isSelectable && locs && e.shiftKey) {
            locs.setSelected(settings.loc, on => !on)
            e.stopPropagation()
            e.preventDefault()
            return true
        }
        return false
    }

    const onPointerDown = (e: React.PointerEvent<HTMLSpanElement>) => {
        // We have special handling for shift+click events, so prevent default browser
        // events from interfering when shift is held.
        if (settings.isSelectable && locs && e.shiftKey) {
            e.preventDefault()
        }
    }

    const dataVscodeContext = {
        // We set both IDs to an invalid value by default
        // in order to clear ancestors' locations in this span
        // (`data-vscode-context` fields are overridden by child components).
        // The value can be anything that will not be returned from `useId`.
        selectableLocationId: '',
        unselectableLocationId: '',
    }
    const id = React.useId()
    if (settings.isSelectable && locs) {
        if (locs.isSelected(settings.loc)) dataVscodeContext.unselectableLocationId = id
        else dataVscodeContext.selectableLocationId = id
    }
    useEvent(
        ec.events.clickedContextMenu,
        _ => {
            if (!settings.isSelectable || !locs) return
            locs.setSelected(settings.loc, true)
        },
        [locs, settings],
        `select:${id}`,
    )
    useEvent(
        ec.events.clickedContextMenu,
        _ => {
            if (!settings.isSelectable || !locs) return
            locs.setSelected(settings.loc, false)
        },
        [locs, settings],
        `unselect:${id}`,
    )

    return {
        className,
        onClick,
        onPointerDown,
        dataVscodeContext,
    }
}

/* ---------------------------------------------------------------------------------------------
 * `goals.tsx`
 * ------------------------------------------------------------------------------------------- */

/** Returns true if `h` is inaccessible according to Lean's default name rendering. */
function isInaccessibleName(h: string): boolean {
    return h.indexOf('✝') >= 0
}

function goalToString(g: InteractiveGoal): string {
    let ret = ''

    if (g.userName) {
        ret += `case ${g.userName}\n`
    }

    for (const h of g.hyps) {
        const names = InteractiveHypothesisBundle_nonAnonymousNames(h).join(' ')
        ret += `${names} : ${TaggedText_stripTags(h.type)}`
        if (h.val) {
            ret += ` := ${TaggedText_stripTags(h.val)}`
        }
        ret += '\n'
    }

    ret += `⊢ ${TaggedText_stripTags(g.type)}`

    return ret
}

export function goalsToString(goals: InteractiveGoals): string {
    return goals.goals.map(goalToString).join('\n\n')
}

export interface GoalSettingsState {
    /** If true reverse the list of hypotheses, if false present the order received from LSP. */
    reverse: boolean
    /** If true hide the names of goals, otherwise show them. */
    hideGoalNames: boolean
    /** If true emphasize the first goal, otherwise don't emphasize it. */
    emphasizeFirstGoal: boolean
    /** If true show hypotheses that have isType=True, otherwise hide them. */
    showType: boolean
    /** If true show hypotheses that have isInstance=True, otherwise hide them. */
    showInstance: boolean
    /** If true show hypotheses that contain a dagger in the name, otherwise hide them. */
    showHiddenAssumption: boolean
    /** If true show the bodies of let-values, otherwise hide them. */
    showLetValue: boolean
}

function goalSettingsStateOfConfig(config: InfoviewConfig): GoalSettingsState {
    return {
        reverse: config.reverseTacticState,
        hideGoalNames: !config.showGoalNames,
        emphasizeFirstGoal: config.emphasizeFirstGoal,
        showType: !config.hideTypeAssumptions,
        showInstance: !config.hideInstanceAssumptions,
        showHiddenAssumption: !config.hideInaccessibleAssumptions,
        showLetValue: !config.hideLetValues,
    }
}

function getFilteredHypotheses(
    hyps: InteractiveHypothesisBundle[],
    settings: GoalSettingsState,
): InteractiveHypothesisBundle[] {
    return hyps.reduce((acc: InteractiveHypothesisBundle[], h) => {
        if (h.isInstance && !settings.showInstance) return acc
        if (h.isType && !settings.showType) return acc
        const names = settings.showHiddenAssumption ? h.names : h.names.filter(n => !isInaccessibleName(n))
        const hNew: InteractiveHypothesisBundle = settings.showLetValue
            ? { ...h, names }
            : { ...h, names, val: undefined }
        if (names.length !== 0) acc.push(hNew)
        return acc
    }, [])
}

export interface HypNameProps {
    name: string
    isInserted: boolean
    isRemoved: boolean
    mvarId?: MVarId | undefined
    fvarId?: FVarId | undefined
}

export function HypName({ name, isInserted, isRemoved, mvarId, fvarId }: HypNameProps) {
    const ref = React.useRef<HTMLSpanElement>(null)

    const locs = React.useContext(LocationsContext)

    const hhl = useHoverHighlight({
        ref,
        highlightOnHover: locs !== undefined && mvarId !== undefined && fvarId !== undefined,
        underlineOnModHover: false,
    })

    let selectableLocationSettings: SelectableLocationSettings
    if (mvarId !== undefined && fvarId !== undefined) {
        selectableLocationSettings = { isSelectable: true, loc: { mvarId, loc: { hyp: fvarId } } }
    } else {
        selectableLocationSettings = { isSelectable: false }
    }
    const sl = useSelectableLocation(selectableLocationSettings)

    const namecls: string =
        (isInserted ? 'inserted-text ' : '') +
        (isRemoved ? 'removed-text ' : '') +
        (isInaccessibleName(name) ? 'goal-inaccessible ' : '') +
        hhl.className +
        sl.className
    return (
        <>
            <span
                ref={ref}
                className={namecls}
                data-vscode-context={JSON.stringify(sl.dataVscodeContext)}
                onPointerOver={e => hhl.onPointerOver(e)}
                onPointerOut={e => hhl.onPointerOut(e)}
                onPointerMove={e => hhl.onPointerMove(e)}
                onClick={e => sl.onClick(e)}
                onPointerDown={e => sl.onPointerDown(e)}
            >
                {name}
            </span>
            &nbsp;
        </>
    )
}

export interface HypProps {
    hyp: InteractiveHypothesisBundle
    mvarId?: MVarId
}

export function Hyp({ hyp: h, mvarId }: HypProps) {
    const locs = React.useContext(LocationsContext)

    const names = InteractiveHypothesisBundle_nonAnonymousNames(h).map((n, i) => (
        <HypName
            name={n}
            isInserted={!!h.isInserted}
            isRemoved={!!h.isRemoved}
            mvarId={mvarId}
            fvarId={h.fvarIds?.at(i)}
            key={i}
        ></HypName>
    ))

    const typeLocs: Locations | undefined = React.useMemo(
        () =>
            locs && mvarId && h.fvarIds && h.fvarIds.length > 0
                ? { ...locs, subexprTemplate: { mvarId, loc: { hypType: [h.fvarIds[0], ''] } } }
                : undefined,
        [locs, mvarId, h.fvarIds],
    )

    const valLocs: Locations | undefined = React.useMemo(
        () =>
            h.val && locs && mvarId && h.fvarIds && h.fvarIds.length > 0
                ? { ...locs, subexprTemplate: { mvarId, loc: { hypValue: [h.fvarIds[0], ''] } } }
                : undefined,
        [h.val, locs, mvarId, h.fvarIds],
    )

    return (
        <div>
            <strong className="goal-hyp">{names}</strong>
            :&nbsp;
            <LocationsContext.Provider value={typeLocs}>
                <InteractiveCode fmt={h.type} />
            </LocationsContext.Provider>
            {h.val && (
                <LocationsContext.Provider value={valLocs}>
                    &nbsp;:=&nbsp;
                    <InteractiveCode fmt={h.val} />
                </LocationsContext.Provider>
            )}
        </div>
    )
}

export interface GoalProps {
    goal: InteractiveGoal
    settings: GoalSettingsState
    additionalClassNames: string
}

/**
 * Displays the hypotheses, target type and optional case label of a goal according to the
 * provided `filter`. */
export const Goal = React.memo((props: GoalProps) => {
    const { goal, settings, additionalClassNames } = props

    const prefix = goal.goalPrefix ?? '⊢ '
    const filteredList = getFilteredHypotheses(goal.hyps, settings)
    const hyps = settings.reverse ? filteredList.slice().reverse() : filteredList
    const locs = React.useContext(LocationsContext)
    const goalLocs = React.useMemo(
        () =>
            locs && goal.mvarId
                ? { ...locs, subexprTemplate: { mvarId: goal.mvarId, loc: { target: '' } } }
                : undefined,
        [locs, goal.mvarId],
    )
    const goalLi = (
        <div key={'goal'} data-is-goal>
            <strong className="goal-vdash">{prefix}</strong>
            <LocationsContext.Provider value={goalLocs}>
                <InteractiveCode fmt={goal.type} />
            </LocationsContext.Provider>
        </div>
    )

    let cn = 'font-code tl pre-wrap bl bw1 pl1 b--transparent mb3 ' + additionalClassNames
    if (props.goal.isInserted) cn += ' b--inserted '
    if (props.goal.isRemoved) cn += ' b--removed '

    const children: React.ReactNode[] = [
        settings.reverse && goalLi,
        hyps.map((h, i) => <Hyp hyp={h} mvarId={goal.mvarId} key={i} />),
        !settings.reverse && goalLi,
    ]

    if (goal.userName && !settings.hideGoalNames) {
        return (
            <details open className={cn}>
                <summary
                    className="mv1 pointer"
                    onClick={e => preventClickOnTextSelection(e)}
                    onMouseDown={e => preventDoubleClickTextSelection(e)}
                >
                    <strong className="goal-case">case </strong>
                    {goal.userName}
                </summary>
                {children}
            </details>
        )
    } else return <div className={cn}>{children}</div>
})

export interface GoalsProps {
    goals: InteractiveGoals
    settings: GoalSettingsState
    /** Whether or not to display the number of goals. */
    displayCount: boolean
}

export function Goals({ goals, settings, displayCount }: GoalsProps) {
    const nGoals = goals.goals.length
    if (nGoals === 0) {
        return <strong className="db2 mb2 goal-goals">No goals</strong>
    } else {
        const unemphasizeCn = 'o-70 font-size-code-smaller'
        return (
            <>
                {displayCount && (
                    <strong className="db mb2 goal-goals">
                        {nGoals} {1 < nGoals ? 'goals' : 'goal'}
                    </strong>
                )}
                {goals.goals.map((g, i) => (
                    <Goal
                        key={i}
                        goal={g}
                        settings={settings}
                        additionalClassNames={i !== 0 && settings.emphasizeFirstGoal ? unemphasizeCn : ''}
                    />
                ))}
            </>
        )
    }
}

export interface FilteredGoalsProps {
    /** Components to render in the header.
     *
     * NOTE(deviation): this is `headerChildren` in the original.
     * We use `children` instead,
     * because that is how ProofWidgets passes nested `Html` to a component. */
    children?: React.ReactNode
    /**
     * When this is `undefined`, the component will not appear at all but will remember its state
     * by virtue of still being mounted in the React tree. When it does appear again, the filter
     * settings and collapsed state will be as before. */
    goals?: InteractiveGoals
    /** Whether or not to display the number of goals. */
    displayCount: boolean
    /** Whether the list of goals should be expanded on first render. */
    initiallyOpen: boolean
    /** If specified, the display will be toggled (collapsed/expanded) when this action is requested
     * by the user. */
    togglingAction?: InfoviewActionKind
}

/**
 * Display goals together with a header containing the provided children as well as buttons
 * to control how the goals are displayed.
 */
export const FilteredGoals = React.memo(
    ({ children: headerChildren, goals, displayCount, initiallyOpen, togglingAction }: FilteredGoalsProps) => {
        const ec = React.useContext(EditorContext)
        const config = useInfoviewConfig()

        const [goalSettings, setGoalSettings] = React.useState<GoalSettingsState>(goalSettingsStateOfConfig(config))

        const goalSettingsDifferFromDefaultConfig =
            JSON.stringify(goalSettings) !== JSON.stringify(goalSettingsStateOfConfig(config))

        const saveConfig = React.useCallback(async () => {
            await ec.api.saveConfig({
                ...config,
                reverseTacticState: goalSettings.reverse,
                showGoalNames: !goalSettings.hideGoalNames,
                emphasizeFirstGoal: goalSettings.emphasizeFirstGoal,
                hideTypeAssumptions: !goalSettings.showType,
                hideInstanceAssumptions: !goalSettings.showInstance,
                hideInaccessibleAssumptions: !goalSettings.showHiddenAssumption,
                hideLetValues: !goalSettings.showLetValue,
            })
        }, [config, ec.api, goalSettings])

        // NOTE(deviation): the original displays a settings gear (`codicon-settings-gear`)
        // which opens a menu of goal settings in a hover tooltip (`WithTooltipOnHover`).
        // The tooltip machinery (`tooltips.tsx`) is not exported from `@leanprover/infoview`
        // and depends on `@floating-ui/react`, so it is omitted here.
        // The same settings remain accessible through the VSCode context menu (see below).

        const context: { [k: string]: any } = {}
        const id = React.useId()
        const useContextMenuEvent = (
            name: string,
            action: () => void,
            isEnabled: boolean,
            dependencies?: React.DependencyList,
        ) => {
            if (isEnabled) {
                context[name + 'Id'] = id
            }
            useEvent(ec.events.clickedContextMenu, _ => action(), dependencies, `${name}:${id}`)
        }
        const useSettingsContextMenuEvent = (name: string, setting: Partial<GoalSettingsState>, isEnabled: boolean) =>
            useContextMenuEvent(name, () => setGoalSettings(s => ({ ...s, ...setting })), isEnabled)

        useSettingsContextMenuEvent('displayTargetBeforeAssumptions', { reverse: true }, !goalSettings.reverse)
        useSettingsContextMenuEvent('displayAssumptionsBeforeTarget', { reverse: false }, goalSettings.reverse)
        useSettingsContextMenuEvent('hideTypeAssumptions', { showType: false }, goalSettings.showType)
        useSettingsContextMenuEvent('showTypeAssumptions', { showType: true }, !goalSettings.showType)
        useSettingsContextMenuEvent('hideInstanceAssumptions', { showInstance: false }, goalSettings.showInstance)
        useSettingsContextMenuEvent('showInstanceAssumptions', { showInstance: true }, !goalSettings.showInstance)
        useSettingsContextMenuEvent(
            'hideInaccessibleAssumptions',
            { showHiddenAssumption: false },
            goalSettings.showHiddenAssumption,
        )
        useSettingsContextMenuEvent(
            'showInaccessibleAssumptions',
            { showHiddenAssumption: true },
            !goalSettings.showHiddenAssumption,
        )
        useSettingsContextMenuEvent('hideLetValues', { showLetValue: false }, goalSettings.showLetValue)
        useSettingsContextMenuEvent('showLetValues', { showLetValue: true }, !goalSettings.showLetValue)
        useSettingsContextMenuEvent('hideGoalNames', { hideGoalNames: true }, !goalSettings.hideGoalNames)
        useSettingsContextMenuEvent('showGoalNames', { hideGoalNames: false }, goalSettings.hideGoalNames)
        useSettingsContextMenuEvent(
            'emphasizeFirstGoal',
            { emphasizeFirstGoal: true },
            !goalSettings.emphasizeFirstGoal,
        )
        useSettingsContextMenuEvent(
            'deemphasizeFirstGoal',
            { emphasizeFirstGoal: false },
            goalSettings.emphasizeFirstGoal,
        )
        useContextMenuEvent('saveSettings', () => saveConfig(), goalSettingsDifferFromDefaultConfig, [saveConfig])
        useContextMenuEvent(
            'copyState',
            () => {
                if (goals !== undefined) {
                    void ec.api.copyToClipboard(goalsToString(goals))
                }
            },
            goals !== undefined,
        )

        const setOpenRef = React.useRef<React.Dispatch<React.SetStateAction<boolean>>>()
        useEvent(
            ec.events.requestedAction,
            _ => {
                if (togglingAction !== undefined && setOpenRef.current !== undefined) {
                    setOpenRef.current(t => !t)
                }
            },
            [setOpenRef, togglingAction],
            togglingAction,
        )

        return (
            <div style={goals !== undefined ? {} : { display: 'none' }} data-vscode-context={JSON.stringify(context)}>
                <Details setOpenRef={r => (setOpenRef.current = r)} initiallyOpen={initiallyOpen}>
                    <>{headerChildren}</>
                    <div className="ml1">
                        {goals && <Goals goals={goals} settings={goalSettings} displayCount={displayCount}></Goals>}
                    </div>
                </Details>
            </div>
        )
    },
)

/* ---------------------------------------------------------------------------------------------
 * `info.tsx`
 * ------------------------------------------------------------------------------------------- */

/**
 * The goal-related part of an info block (`GoalInfoDisplay` in `info.tsx`):
 * the tactic state and the expected type.
 *
 * This is the `default` export so that it can be used as a panel widget.
 * NOTE(deviation): the original takes `goals?: InteractiveGoals`,
 * but panel widgets receive `goals: InteractiveGoal[]`,
 * which is `[]` both when there are no goals and when there is no tactic state at all.
 * We cannot distinguish these cases, so we always show the tactic state
 * (the original hides it in the latter case).
 * NOTE(deviation): the original also displays panel widgets here; we do not.
 */
export default function GoalInfoDisplay(props: PanelWidgetProps) {
    const { pos, termGoal } = props
    const goals: InteractiveGoals = React.useMemo(() => ({ goals: props.goals }), [props.goals])
    const ec = React.useContext(EditorContext)

    const config = useInfoviewConfig()

    // NOTE(deviation): the selection is local to this display.
    // The infoview's own selection (passed to panel widgets in `props.selectedLocations`)
    // is read-only from our point of view,
    // and locations selected here are not communicated to other panel widgets.
    const [selectedLocs, setSelectedLocs] = React.useState<GoalsLocation[]>([])
    const selectedLocationsId = React.useId()
    useEvent(
        ec.events.clickedContextMenu,
        _ => {
            setSelectedLocs([])
        },
        [setSelectedLocs],
        `unselectAll:${selectedLocationsId}`,
    )

    // https://react.dev/learn/you-might-not-need-an-effect#adjusting-some-state-when-a-prop-changes
    const [prevPos, setPrevPos] = React.useState<DocumentPosition>(pos)
    if (!DocumentPosition.isEqual(pos, prevPos)) {
        setPrevPos(pos)
        setSelectedLocs([])
    }

    const locs: Locations = React.useMemo(
        () => ({
            isSelected: (l: GoalsLocation) => selectedLocs.some(v => GoalsLocation.isEqual(v, l)),
            setSelected: (l, act) =>
                setSelectedLocs(ls => {
                    // We ensure that `selectedLocs` maintains its reference identity if the selection
                    // status of `l` didn't change.
                    const newLocs = ls.filter(v => !GoalsLocation.isEqual(v, l))
                    const wasSelected = newLocs.length !== ls.length
                    const isSelected = typeof act === 'function' ? act(wasSelected) : act
                    if (isSelected) newLocs.push(l)
                    return wasSelected === isSelected ? ls : newLocs
                }),
            subexprTemplate: undefined,
        }),
        [selectedLocs],
    )

    return (
        <>
            <LocationsContext.Provider value={locs}>
                <span data-vscode-context={JSON.stringify(selectedLocs.length === 0 ? {} : { selectedLocationsId })}>
                    <FilteredGoals key="goals" initiallyOpen goals={goals} displayCount>
                        Tactic state
                    </FilteredGoals>
                </span>
            </LocationsContext.Provider>
            {config.expectedTypeVisibility !== 'Hidden' && (
                <FilteredGoals
                    key="term-goal"
                    goals={termGoal !== undefined ? { goals: [termGoal] } : undefined}
                    initiallyOpen={config.expectedTypeVisibility === 'Expanded by default'}
                    displayCount={false}
                    togglingAction="toggleExpectedType"
                >
                    Expected type
                </FilteredGoals>
            )}
        </>
    )
}
