namespace Tracer.Benchmarks

open System

module ScenePolicy =
    let primaryIds = [ "chair"; "roman-bust"; "space-sentinel"; "sky-arena" ]
    let extraIds = [ "gold-dragon" ]

    let selectScenes (selection: string) =
        if selection = "all" then List.toArray primaryIds
        else
            let ids = selection.Split(',', StringSplitOptions.RemoveEmptyEntries ||| StringSplitOptions.TrimEntries)
            let allowed = Set.ofList (primaryIds @ extraIds)
            if ids.Length = 0 || (Array.distinct ids).Length <> ids.Length || ids |> Array.exists (allowed.Contains >> not) then
                invalidArg (nameof selection) $"""Expected all or a unique selection from {String.concat "," (primaryIds @ extraIds)}."""
            ids

    let usesProceduralAssets ids =
        ids |> Array.exists (fun id -> List.contains id primaryIds)

    let validateVariants sceneId variants =
        if sceneId = "gold-dragon" && variants <> [| "authored" |] then
            invalidArg "materials" "gold-dragon is an extra authored/gold-only case. Use --materials authored; the open scan must not enter the material/glass sweep."
