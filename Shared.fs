module Shared 

    let explode (s:string) = [for c in s -> c]
    let implode (xs:char list) =
            let sb = System.Text.StringBuilder(xs.Length)
            xs |> List.iter (sb.Append >> ignore)
            sb.ToString()

    let addOrUpdate initial update key map = Map.add key (match map |> Map.tryFind key with | Some x -> update x | None -> initial) map

