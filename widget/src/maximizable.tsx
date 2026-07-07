/*
Copyright (c) 2026 Lean FRO, LLC. All rights reserved.
Released under Apache 2.0 license as described in the file LICENSE.
Authors: Wojciech Nawrocki
*/

import * as React from 'react'

export default function Maximizable({ children }: { children: React.ReactNode }) {
  const divRef = React.useRef<HTMLDivElement>(null)
  const [isMaximized, setIsMaximized] = React.useState(false)
  const divStyle: React.CSSProperties = isMaximized ? {
      // Take up entire viewport
      position: 'fixed',
      top: 0,
      left: 0,
      width: '100vw',
      height: '100vh',
      // Draw over other elements
      zIndex: 10_000,
      background: 'var(--vscode-editor-background)',
    } : {
      position: 'relative',
      background: 'var(--vscode-editor-background)',
    }

  const firstRectRef = React.useRef<DOMRect>(undefined)
  /** Animate from `firstRectRef` to the current location,
   * as in https://css-tricks.com/animating-layouts-with-the-flip-technique/.
   * We use real positioning instead of transform - the latter is mispositioned. */
  React.useLayoutEffect(() => {
    if (!divRef.current || !firstRectRef.current) return

    const div = divRef.current
    const first = firstRectRef.current
    const last = div.getBoundingClientRect()

    const prevPosition = div.style.position
    div.style.position = 'fixed'
    const animation = div.animate(
      [
        {
          top: `${first.top}px`,
          left: `${first.left}px`,
          width: `${first.width}px`,
          height: `${first.height}px`,
        },
        {
          top: `${last.top}px`,
          left: `${last.left}px`,
          width: `${last.width}px`,
          height: `${last.height}px`,
        },
      ],
      {
        duration: 160,
        easing: 'ease-out',
      }
    )
    animation.finished.then(() => {
      div.style.position = prevPosition
    })

    return () => {
      animation.cancel()
    }
  }, [isMaximized])

  const label = isMaximized ? 'Restore' : 'Maximize'
  const codiconClass = isMaximized ? 'codicon-screen-normal ' : 'codicon-screen-full '

  return (
    <div
      ref={divRef}
      style={divStyle}
      className={isMaximized ? 'pa1 ' : ''}
    >
      <a
        className={'br2 pointer dim codicon ' + codiconClass}
        style={{
          background: 'var(--vscode-editor-background)',
          // Top-right corner
          position: 'absolute',
          top: '0.35rem',
          right: '0.35rem',
        }}
        role='button'
        title={label}
        aria-label={label}
        aria-pressed={isMaximized}
        onClick={ev => {
          ev.preventDefault()
          firstRectRef.current = divRef.current?.getBoundingClientRect()
          setIsMaximized(b => !b)
        }}
      />
      {children}
    </div>
  )
}
