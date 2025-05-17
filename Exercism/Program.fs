module ExercismExercices


let mapRNA s =
        match s with
        | "AUG" -> "Methionine"
        | "UUU" | "UUC" -> "Phenylalanine"
        | "UUA" | "UUG" -> "Leucine"
        | "UCU" | "UCC" | "UCA" |"UCG" -> "Serine"
        | "UAU" | "UAC" -> "Tyrosine"
        | "UGU" | "UGC" -> "Cysteine"
        | "UGG" -> "Tryptophan"
        | "UAA" | "UAG" | "UGA" -> "Stop"
        | _ -> ""
        
let proteins (rna: string) =
    let rec aux (rn : string) acc =
        match rn with
        | "" -> acc
        | _ -> 
            let sub = mapRNA rn[0..2]
            match sub with
            | "Stop" -> acc
            | _ -> 
                let rest = rn[ 3 .. ]
                aux rest (sub :: acc) 
    (aux rna []) |> List.rev  
    