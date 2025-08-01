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
