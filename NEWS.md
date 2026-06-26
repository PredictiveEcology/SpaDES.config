# SpaDES.config (development version)

* drop support for R 4.2 due to changes in dependency packages;
* remove suggested dependencies `fireSenseUtils` and `scfmutils` (#1);
* remove the `SpaDES.project` dependency and the `getModule()` shim it powered: the shim is redundant because the relevant modules already guard their own `getModule()` calls against existing project modules, so this drops a hard-to-build dependency from the package;
* add `from_wd` argument to `findProjectPath`;
* remove `tilePath` from default paths;
* add `sharedOutputPath` to default paths;
* message instead of warning for ignored module parameters;
* `.getRelativePath()` falls back to a plain relative path instead of erroring (and returning NA-filled garbage) when no path component is shared with the project directory, or when the deepest shared component is the path leaf;
* documentation improvements;
* update and add new tests;

# SpaDES.config 1.0.7

* require `scfmutils >= 0.0.13`;
* rename `.needPkg()` to `needPkg()` and export;
* export `modList()`;

# SpaDES.config 1.0.6

* add `box` to Suggests for use with tests;
* fix issue with `getModule()` shim creation;
* minor improvements;

# SpaDES.config 1.0.5

* implemented shim for `SpaDES.project::getModule()` which now redirects to `use_project_module()` (requires `SpaDES.project` to be loaded, so added to Depends);
* renamed our `getModule()` to `use_project_module()`;
* improved documentation;

# SpaDES.config 1.0.2

* removed `tools` from Imports;
* removed `LandWebUtils` from Suggests;
* moved project-specific configs to their respective projects, and removed `useConfig()` and `useContext()` in favour of using `box::use()` from a project;
* new function `getModule()` to override `SpaDES.project::getModule()`;
* improved handling of absolute and relative paths;

# SpaDES.config 0.0.3

* removed `Require` from Imports and reimplemented an internal version of `normPath()` that uses `fs` functions and properly handles symlinks;
* removed `SpaDES.project` from Imports and reimplemented custom versions of `findProjectPath()` and `findProjectName()`;
* new helper functions `user()` and `machine()`;
* cloud caching disabled in project configs by default until `reproducible::cloudCache()` is fixed to no longer spam the cloud folder with temp directories;
* misc project config updates;

# SpaDES.config 0.0.1

* initial version
