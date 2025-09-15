import Lake
open Lake DSL System

package proofwidgets where
  preferReleaseBuild := true
  buildArchive? := "ProofWidgets4.tar.gz"
  releaseRepo := "https://github.com/leanprover-community/ProofWidgets4"

require "leanprover-community" / "batteries" @ git "v4.24.0-rc1"

def widgetDir : FilePath := "widget"

nonrec def Lake.Package.widgetDir (pkg : Package) : FilePath :=
  pkg.dir / widgetDir

def Lake.Package.runNpmCommand (pkg : Package) (args : Array String) : LogIO Unit :=
  -- Running `cmd := "npm.cmd"` directly fails on Windows sometimes
  -- (https://github.com/leanprover-community/ProofWidgets4/issues/97)
  -- so run in PowerShell instead (`cmd.exe` also doesn't work.)
  if Platform.isWindows then
    proc {
      cmd := "powershell"
      args := #["-Command", "npm.cmd"] ++ args
      cwd := some pkg.widgetDir
    } (quiet := true) -- use `quiet` here or `lake` will replay the output in downstream projects.
  else
    proc {
      cmd := "npm"
      args
      cwd := some pkg.widgetDir
    } (quiet := true)

input_file widgetPackageJson where
  path := widgetDir / "package.json"
  text := true

/-- Target to update `package-lock.json` whenever `package.json` has changed. -/
target widgetPackageLock pkg : FilePath := do
  let packageFile ← widgetPackageJson.fetch
  let packageLockFile := pkg.widgetDir / "package-lock.json"
  buildFileAfterDep (text := true) packageLockFile packageFile fun _srcFile => do
    pkg.runNpmCommand #["install"]

input_file widgetRollupConfig where
  path := widgetDir / "rollup.config.js"
  text := true

input_file widgetTsconfig where
  path := widgetDir / "tsconfig.json"
  text := true

/-- The TypeScript widget modules in `widget/src`. -/
input_dir widgetJsSrcs where
  path := widgetDir / "src"
  filter := .extension <| .mem #["ts", "tsx", "js", "jsx"]
  text := true

/-- Target to build all widget modules from `widgetJsSrcs`. -/
def widgetJsAllTarget (pkg : Package) (isDev : Bool) : FetchM (Job Unit) := do
  let srcs ← widgetJsSrcs.fetch
  let rollupConfig ← widgetRollupConfig.fetch
  let tsconfig ← widgetTsconfig.fetch
  let widgetPackageLock ← widgetPackageLock.fetch
  /- `widgetJsAll` is built via `needs`,
  and Lake's default build order is `needs -> cloud release -> main build`.
  We must instead ensure that the cloud release is fetched first
  so that this target does not build from scratch unnecessarily.
  `afterBuildCacheAsync` guarantees this. -/
  pkg.afterBuildCacheAsync do
  srcs.bindM (sync := true) fun _ =>
  rollupConfig.bindM (sync := true) fun _ =>
  tsconfig.bindM (sync := true) fun _ =>
  widgetPackageLock.mapM fun _ => do
    let traceFile := pkg.buildDir / "js" / "lake.trace"
    buildUnlessUpToDate traceFile (← getTrace) traceFile do
      if let some msg := get_config? errorOnBuild then
        error msg
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
      pkg.runNpmCommand #["clean-install"]
      pkg.runNpmCommand #["run", if isDev then "build-dev" else "build"]

target widgetJsAll pkg : Unit :=
  widgetJsAllTarget pkg (isDev := false)

target widgetJsAllDev pkg : Unit :=
  widgetJsAllTarget pkg (isDev := true)

@[default_target]
lean_lib ProofWidgets where
  needs := #[widgetJsAll]

lean_lib ProofWidgets.Demos where
  needs := #[widgetJsAll]
  globs := #[.submodules `ProofWidgets.Demos]
