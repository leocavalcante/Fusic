type note =
    | C
    | Db // C#
    | D
    | Eb // D#
    | E
    | F
    | Gb // F#
    | G
    | Ab // G#
    | A
    | Bb // A#
    | B

type Interval = note -> note

let semitone : Interval =
    function 
    | C -> Db
    | Db -> D
    | D -> Eb
    | Eb -> E
    | E -> F
    | F -> Gb
    | Gb -> G
    | G -> Ab
    | Ab -> A
    | A -> Bb
    | Bb -> B
    | B -> C

let tone : Interval =
    function 
    | C -> D
    | Db -> Eb
    | D -> E
    | Eb -> F
    | E -> Gb
    | F -> G
    | Gb -> Ab
    | G -> A
    | Ab -> Bb
    | A -> B
    | Bb -> C
    | B -> Db

type triad =
    | I of note
    | II of note
    | III of note
    | IV of note
    | V of note
    | VI of note
    | VII of note

let major = [ tone; tone; semitone; tone; tone; tone; semitone ]

let chord (scale : triad list) =
    match scale with
    | [ I tonic; II _; III mediant; IV _; V dominant; VI _; VII _; I _ ] -> 
        [ tonic; mediant; dominant ]
    | _ -> []

let scale (intervals : Interval list) (tonic : note) =
    intervals
    |> List.fold (fun notes interval -> 
           notes @ [ (match (List.last notes) with
                      | I tonic -> II(interval tonic)
                      | II supertonic -> III(interval supertonic)
                      | III mediant -> IV(interval mediant)
                      | IV subdominant -> V(interval subdominant)
                      | V dominant -> VI(interval dominant)
                      | VI submediant -> VII(interval submediant)
                      | VII leadingTone -> I(interval leadingTone)) ]) 
           [ I tonic ]

let noteFromString =
    function 
    | "C" -> Some C
    | "Db" -> Some Db
    | "D" -> Some D
    | "Eb" -> Some Eb
    | "E" -> Some E
    | "F" -> Some F
    | "Gb" -> Some Gb
    | "G" -> Some G
    | "Ab" -> Some Ab
    | "A" -> Some A
    | "Bb" -> Some Bb
    | "B" -> Some B
    | _ -> None

[<EntryPoint>]
let main args =
    let input = args.[0]
    let note = input |> noteFromString
    match note with
    | Some note -> 
        let scale = scale major note
        let chord = chord scale
        printfn "Scale: %A\nChord: %A" scale chord
    | None -> printf "Note %s not recognized" input
    0
