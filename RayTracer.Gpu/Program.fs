namespace Tracer.Gpu

open System
open System.Text.Json

module Program =
    [<EntryPoint>]
    let main arguments =
        try
            match arguments with
            | [| "--gate"; report |] ->
                FeasibilityGate.run report
                0
            | [| "--self-test"; report |] ->
                SelfTests.run report
                0
            | [| "--edge-test"; report |] ->
                GeometryTests.run report
                0
            | [| "--compare-core"; scene; material; settings; report |] ->
                CoreComparison.run scene material settings report
                0
            | [| "--help" |] ->
                printfn "%s\nDiagnostics: --gate <report.json>, --self-test <report.json>, --edge-test <report.json>, --compare-core <scene.json> <material> <settings.json> <report.json>" Worker.usage
                0
            | values -> Worker.run values
        with error ->
            eprintfn "%s" (JsonSerializer.Serialize({| status = "error"; backend = "cuda"; error = error.ToString() |}))
            1
