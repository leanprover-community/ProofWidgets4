import Lake
open Lake DSL System

package proofwidgets where
  preferReleaseBuild := true
  buildArchive? := "ProofWidgets4.tar.gz"
  releaseRepo := "https://github.com/mars0i/ProofWidgets4"
  -- releaseRepo := "https://github.com/leanprover-community/ProofWidgets4"

require "leanprover-community" / "batteries" @ git "v4.16.0-rc2"

def npmCmd : String :=
  if Platform.isWindows then "npm.cmd" else "npm"

def Lake.Package.widgetDir (pkg : Package) := pkg.dir / "widget"

/-- Target to update `package-lock.json` whenever `package.json` has changed. -/
target widgetPackageLock pkg : FilePath := do
  let packageFile ← inputTextFile <| pkg.widgetDir / "package.json"
  let packageLockFile := pkg.widgetDir / "package-lock.json"
  buildFileAfterDep (text := true) packageLockFile packageFile fun _srcFile => do
    proc {
      cmd := npmCmd
      args := #["install"]
      cwd := some pkg.widgetDir
    } (quiet := true)

/-- Target to build all TypeScript widget modules that match `widget/src/*.tsx`. -/
def widgetJsAllTarget (pkg : Package) (isDev : Bool) : FetchM (Job (Array FilePath)) := do
  let fs ← (pkg.widgetDir / "src").readDir
  let srcs : Array FilePath := fs.filterMap fun f =>
    let p := f.path
    match p.extension with
    | some "ts" | some "tsx" | some "js" | some "jsx" => some p
    | _ => none
  -- See https://leanprover.zulipchat.com/#narrow/stream/113488-general/topic/ProofWidgets.20v0.2E0.2E36.20and.20v0.2E0.2E39/near/446602029
  let srcs := srcs.qsort (toString · < toString ·)
  let depFiles := srcs ++ #[ pkg.widgetDir / "rollup.config.js", pkg.widgetDir / "tsconfig.json" ]
  let deps ← liftM <| depFiles.mapM inputTextFile
  let deps := deps.push <| ← widgetPackageLock.fetch
  let deps := Job.collectArray deps
  /- `widgetJsAll` is an `extraDepTarget`,
  and Lake's default build order is `extraDepTargets -> cloud release -> main build`.
  We must instead ensure that the cloud release is fetched first
  so that this target does not build from scratch unnecessarily.
  `afterReleaseAsync` guarantees this. -/
  pkg.afterBuildCacheAsync $ deps.mapM fun depInfo => do
    let traceFile := pkg.buildDir / "js" / "lake.trace"
    let _ ← buildUnlessUpToDate? traceFile (← getTrace) traceFile do
       /- HACK: Ensure that NPM modules are installed before building TypeScript,
       *if* we are building Typescript.
       It would probably be better to have a proper target for `node_modules`
       that all the JS/TS modules depend on.
       BUT when we are being built as a dependency of another package
       using the cloud releases feature,
       we wouldn't want that target to trigger
       since in that case NPM is not necessarily installed.
       Hence, we put this block inside the build process for JS/TS files
       rather than as a top-level target.
       This only runs when some TypeScript needs building. -/
      proc {
        cmd  := npmCmd
        args := #["clean-install"]
        cwd  := some pkg.widgetDir
      } (quiet := true) -- use `quiet` here or `lake` will replay the output in downstream projects.
      proc {
        cmd  := npmCmd
        args := if isDev then #["run", "build-dev"]
                else #["run", "build"]
        cwd  := some pkg.widgetDir
      } (quiet := true) -- use `quiet` here or `lake` will replay the output in downstream projects.
    -- 2024-06-04 (@semorrison): I've commented this out, as `lake` now replays it in every build
    -- including in downstream projects.
    -- if upToDate then
    --   Lake.logInfo "JavaScript build up to date"
    return depInfo

target widgetJsAll pkg : Array FilePath :=
  widgetJsAllTarget pkg (isDev := false)

target widgetJsAllDev pkg : Array FilePath :=
  widgetJsAllTarget pkg (isDev := true)

@[default_target]
lean_lib ProofWidgets where
  extraDepTargets := #[``widgetJsAll]

lean_lib ProofWidgets.Demos where
  globs := #[.submodules `ProofWidgets.Demos]
  extraDepTargets := #[``widgetJsAll]
