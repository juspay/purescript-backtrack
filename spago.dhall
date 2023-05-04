{ name = "backtrack"
, dependencies = [ "effect", "prelude", "transformers" , "control" , "tailrec" ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs"]
}
