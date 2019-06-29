module Shared 

    let explode (s:string) = [for c in s -> c]
    let implode (xs:char list) =
            let sb = System.Text.StringBuilder(xs.Length)
            xs |> List.iter (sb.Append >> ignore)
            sb.ToString()

    let addOrUpdate initial update key map = Map.add key (match map |> Map.tryFind key with | Some x -> update x | None -> initial) map

    let addToListIfNotPresentInSet set list x = if Set.contains x set then list else x :: list

    let mergeMaps map1 map2 = Map.fold (fun acc key value -> Map.add key value acc) map1 map2
