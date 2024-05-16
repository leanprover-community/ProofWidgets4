import Lake
open Lake DSL System

package proofwidgets where
  preferReleaseBuild := true
  buildArchive? := "ProofWidgets4.tar.gz"

require batteries from git "https://github.com/leanprover-community/batteries" @ "main"

def npmCmd : String :=
  if Platform.isWindows then "npm.cmd" else "npm"

def widgetDir := __dir__ / "widget"
def buildDir := __dir__ / ".lake" / "build"

/-- Target to update `package-lock.json` whenever `package.json` has changed. -/
target widgetPackageLock : FilePath := do
  let packageFile ← inputFile <| widgetDir / "package.json"
  let packageLockFile := widgetDir / "package-lock.json"
  buildFileAfterDep packageLockFile packageFile fun _srcFile => do
    proc {
      cmd := npmCmd
      args := #["install"]
      cwd := some widgetDir
    }

/-- Target to build all TypeScript widget modules that match `widget/src/*.tsx`. -/
def widgetJsAllTarget (isDev : Bool) : FetchM (BuildJob (Array FilePath)) := do
  let fs ← (widgetDir / "src").readDir
  let srcs : Array FilePath := fs.filterMap fun f =>
    let p := f.path
    match p.extension with
    | some "ts" | some "tsx" | some "js" | some "jsx" => some p
    | _ => none
  let depFiles := srcs ++ #[ widgetDir / "rollup.config.js", widgetDir / "tsconfig.json" ]
  let deps ← liftM <| depFiles.mapM inputFile
  let deps := deps.push <| ← widgetPackageLock.fetch
  let deps ← BuildJob.collectArray deps
  deps.bindSync fun depInfo depTrace => do
    let traceFile := buildDir / "js" / "lake.trace"
    let upToDate ← buildUnlessUpToDate? traceFile depTrace traceFile do
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
        cwd  := some widgetDir
      }
      proc {
        cmd  := npmCmd
        args := if isDev then #["run", "build-dev"]
                else #["run", "build"]
        cwd  := some widgetDir
      }
    if upToDate then
      Lake.logInfo "JavaScript build up to date"
    return (depInfo, depTrace)

target widgetJsAll : Array FilePath :=
  widgetJsAllTarget (isDev := false)

target widgetJsAllDev : Array FilePath :=
  widgetJsAllTarget (isDev := true)

@[default_target]
lean_lib ProofWidgets where
  extraDepTargets := #[``widgetJsAll]

lean_lib ProofWidgets.Demos where
  globs := #[.submodules `ProofWidgets.Demos]
  extraDepTargets := #[``widgetJsAll]
