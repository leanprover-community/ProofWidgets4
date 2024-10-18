import Lake
open Lake DSL System

package proofwidgets where
  preferReleaseBuild := true
  buildArchive? := "ProofWidgets4.tar.gz"
  releaseRepo := "https://github.com/leanprover-community/ProofWidgets4"

require batteries from git "https://github.com/leanprover-community/batteries" @ "main"

def npmCmd : String :=
  if Platform.isWindows then "npm.cmd" else "npm"

def widgetDir := __dir__ / "widget"
def buildDir := __dir__ / ".lake" / "build"

-- TODO: rm this and use the Lake version when it becomes available in a Lean release
def inputTextFile' (path : FilePath) : SpawnM (BuildJob FilePath) :=
  Job.async do (path, ·) <$> computeTrace (TextFilePath.mk path)

def fetchTextFileHash (file : FilePath) : JobM Hash := do
  let hashFile := FilePath.mk <| file.toString ++ ".hash"
  if (← getTrustHash) then
    if let some hash ← Hash.load? hashFile then
      return hash
  let hash ← computeHash (TextFilePath.mk file)
  createParentDirs hashFile
  IO.FS.writeFile hashFile hash.toString
  return hash

def fetchTextFileTrace (file : FilePath) : JobM BuildTrace := do
  return .mk (← fetchTextFileHash file) (← getMTime file)

def buildTextFileUnlessUpToDate
  (file : FilePath) (depTrace : BuildTrace) (build : JobM PUnit)
: JobM BuildTrace := do
  let traceFile := FilePath.mk <| file.toString ++ ".trace"
  buildUnlessUpToDate file depTrace traceFile do
    build
    clearFileHash file
  fetchTextFileTrace file

/-- Like `buildFileAfterDep` but interprets `file` as a text file
so that line ending differences across platform do not impact the hash. -/
@[inline] def buildTextFileAfterDep
  (file : FilePath) (dep : BuildJob α) (build : α → JobM PUnit)
  (extraDepTrace : JobM _ := pure BuildTrace.nil)
: SpawnM (BuildJob FilePath) :=
  dep.bindSync fun depInfo depTrace => do
    let depTrace := depTrace.mix (← extraDepTrace)
    let trace ← buildTextFileUnlessUpToDate file depTrace <| build depInfo
    return (file, trace)

/-- Target to update `package-lock.json` whenever `package.json` has changed. -/
target widgetPackageLock : FilePath := do
  let packageFile ← inputTextFile' <| widgetDir / "package.json"
  let packageLockFile := widgetDir / "package-lock.json"
  buildTextFileAfterDep packageLockFile packageFile fun _srcFile => do
    proc {
      cmd := npmCmd
      args := #["install"]
      cwd := some widgetDir
    } (quiet := true)

/-- Target to build all TypeScript widget modules that match `widget/src/*.tsx`. -/
def widgetJsAllTarget (pkg : Package) (isDev : Bool) : FetchM (BuildJob (Array FilePath)) := do
  let fs ← (widgetDir / "src").readDir
  let srcs : Array FilePath := fs.filterMap fun f =>
    let p := f.path
    match p.extension with
    | some "ts" | some "tsx" | some "js" | some "jsx" => some p
    | _ => none
  -- See https://leanprover.zulipchat.com/#narrow/stream/113488-general/topic/ProofWidgets.20v0.2E0.2E36.20and.20v0.2E0.2E39/near/446602029
  let srcs := srcs.qsort (toString · < toString ·)
  let depFiles := srcs ++ #[ widgetDir / "rollup.config.js", widgetDir / "tsconfig.json" ]
  let deps ← liftM <| depFiles.mapM inputTextFile'
  let deps := deps.push <| ← widgetPackageLock.fetch
  let deps ← BuildJob.collectArray deps
  /- `widgetJsAll` is an `extraDepTarget`,
  and Lake's default build order is `extraDepTargets -> cloud release -> main build`.
  We must instead ensure that the cloud release is fetched first
  so that this target does not build from scratch unnecessarily.
  `afterReleaseAsync` guarantees this. -/
  pkg.afterBuildCacheAsync $ deps.bindSync fun depInfo depTrace => do
    let traceFile := buildDir / "js" / "lake.trace"
    let _ ← buildUnlessUpToDate? traceFile depTrace traceFile do
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
      } (quiet := true) -- use `quiet` here or `lake` will replay the output in downstream projects.
      proc {
        cmd  := npmCmd
        args := if isDev then #["run", "build-dev"]
                else #["run", "build"]
        cwd  := some widgetDir
      } (quiet := true) -- use `quiet` here or `lake` will replay the output in downstream projects.
    -- 2024-06-04 (@semorrison): I've commented this out, as `lake` now replays it in every build
    -- including in downstream projects.
    -- if upToDate then
    --   Lake.logInfo "JavaScript build up to date"
    return (depInfo, depTrace)

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
