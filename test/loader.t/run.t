Loading findlib packages with Ocsigen_base.Loader.loadfiles. The hooks [pre]
and [post] surround the whole list of files, so the dependencies of a package
(dep2 here) see the configuration of the package. A file loaded with [force],
as the last file of a package is (dep in the first call), is remembered, so a
later request for it as a dependency of another package does not load it a
second time, which Dynlink would refuse.

  $ ../loader/test.bc ../loader/dep.cma ../loader/dep2.cma ../loader/app.cma
  pre
  dep loaded, config = set
  post
  pre
  dep2 loaded, config = set
  app loaded, config = set
  post
