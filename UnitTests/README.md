# Retained assertion harness

Build and run on the SDK pinned by `global.json`:

```sh
dotnet run --project UnitTests --configuration Release -- --list
dotnet run --project UnitTests --configuration Release -- --suites image-io,expr-parse
dotnet run --project UnitTests --configuration Release -- --suite bvh --suite shapes
dotnet run --project UnitTests --configuration Release -- --suite lighting-regression
dotnet run --project UnitTests --configuration Release
```

Each suite is an explicit `unit -> unit` function, so listing or selecting suites
does not initialize/run unselected assertions. No keypress is required. The
summary reports assertions and elapsed `Stopwatch` time; an assertion failure or
uncaught suite exception returns exit code **1**. Invalid options or suite names
return **2**. `--help` and `--list` return **0**.

`lighting` remains an alias for `lighting-regression`; selecting both runs the
suite only once. `numerical` aliases the existing `numerics` selector with the
same deduplication behavior.

The historical transformation suite currently contains only commented-out
assertions; its zero-assertion invocation is not evidence of transformation
coverage. New regression modules should be explicitly listed before `Program.fs`
in the project and registered in `Program.fs`, not added via wildcard imports.
