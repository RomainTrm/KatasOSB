module Tests

// Cardinalité : 2
type Joueur = Joueur1 | Joueur2

// Cardinalité : 4
type PointJoueur = 
| Zero
| Quinze
| Trente
| Quarante

// Cardinalité : 4 * 4 + 2 + 2 = 20
type Score = 
| ScoreJoueurs of (PointJoueur * PointJoueur) // Cardinalité 4 * 4
| Avantage of Joueur // Cardinalité 2 
| Jeu of Joueur //Cardinalite 2

let IncrementePoint point = 
    match point with
    | Zero -> Quinze
    | Quinze -> Trente
    | Trente -> Quarante

let joueurGagne joueur score =
    match joueur, score with 
    | _, ScoreJoueurs (Quarante,Quarante) -> Avantage joueur
    | Joueur1, Avantage Joueur1 -> Jeu Joueur1
    | Joueur2, Avantage Joueur2 -> Jeu Joueur2
    | Joueur1, ScoreJoueurs (Quarante,_) -> Jeu Joueur1
    | Joueur2, ScoreJoueurs (_, Quarante) -> Jeu Joueur2
    | Joueur1, ScoreJoueurs (pointJ1,pointJ2) -> ScoreJoueurs (IncrementePoint pointJ1, pointJ2)
    | Joueur2, ScoreJoueurs (pointJ1,pointJ2) -> ScoreJoueurs (pointJ1, IncrementePoint pointJ2)
    | Joueur1, Avantage Joueur2 -> ScoreJoueurs(Quarante, Quarante)
    | Joueur2, Avantage Joueur1 -> ScoreJoueurs(Quarante, Quarante)
    

open Xunit
open Swensen.Unquote

[<Fact>]
let ``Joureur1 à 0 point passe à 15 si il gagne`` () =
    let result = ScoreJoueurs (Zero, Zero) |> joueurGagne Joueur1
    test <@ result = ScoreJoueurs (Quinze, Zero) @>

[<Fact>]
let ``Joureur2 à 0 point passe à 15 si il gagne`` () =
    let result = ScoreJoueurs (Zero, Zero) |> joueurGagne Joueur2
    test <@ result = ScoreJoueurs (Zero, Quinze) @>

[<Fact>]
let ``Joueur2 à 15 et Joureur1 à 0 point, passe à 15 si Joueur1 gagne`` () =
    let result = ScoreJoueurs (Zero, Quinze) |> joueurGagne Joueur1
    test <@ result = ScoreJoueurs (Quinze, Quinze) @>


[<Fact>]
let ``Joueur1 Gagne : (15, 0) => (30, 0)`` () =
    let result = ScoreJoueurs (Quinze, Zero) |> joueurGagne Joueur1
    test <@ result = ScoreJoueurs (Trente, Zero) @>

[<Fact>]
let ``Joueur1 Gagne : (30, 0) => (40, 0)`` () =
    let result = ScoreJoueurs (Trente, Zero) |> joueurGagne Joueur1
    test <@ result = ScoreJoueurs (Quarante, Zero) @>

    

[<Fact>]
let ``Joueur2 Gagne : (0, 15) => (0, 30)`` () =
    let result = ScoreJoueurs (Zero, Quinze) |> joueurGagne Joueur2
    test <@ result = ScoreJoueurs (Zero, Trente) @>

[<Fact>]
let ``Joueur2 Gagne : (0, 30) => (0, 40)`` () =
    let result = ScoreJoueurs (Zero, Trente) |> joueurGagne Joueur2
    test <@ result = ScoreJoueurs (Zero, Quarante) @>
    
[<Fact>]
let ``Joueur1 Gagne : (40, 40) => Avantage Joueur1`` () =
    let result = ScoreJoueurs (Quarante, Quarante) |> joueurGagne Joueur1
    test <@ result = Avantage Joueur1 @>
    
[<Fact>]
let ``Joueur2 Gagne : (40, 40) => Avantage Joueur2`` () =
    let result = ScoreJoueurs (Quarante, Quarante) |> joueurGagne Joueur2
    test <@ result = Avantage Joueur2 @>

[<Fact>]
let ``Joueur1 Gagne : Avantage Joueur 2 => (40, 40))`` () =
    let result = Avantage Joueur2 |> joueurGagne Joueur1
    test <@ result = ScoreJoueurs (Quarante, Quarante) @>

[<Fact>]
let ``Joueur2 Gagne : Avantage Joueur 1 => (40, 40))`` () =
    let result = Avantage Joueur1 |> joueurGagne Joueur2
    test <@ result = ScoreJoueurs (Quarante, Quarante) @>

[<Fact>]
let ``Joueur1 Gagne : Avantage Joueur 1 => Jeu Joueur 1`` () =
    let result = Avantage Joueur1 |> joueurGagne Joueur1
    test <@ result = Jeu Joueur1 @>

[<Fact>]
let ``Joueur2 Gagne : Avantage Joueur 2 => Jeu Joueur 2`` () =
    let result = Avantage Joueur2 |> joueurGagne Joueur2
    test <@ result = Jeu Joueur2 @>


[<Fact>]
let ``Joueur1 Gagne : (40, 0)) => Jeu Joueur 1`` () =
    let result = ScoreJoueurs (Quarante, Zero) |> joueurGagne Joueur1
    test <@ result = Jeu Joueur1 @>

[<Fact>]
let ``Joueur2 Gagne : (30, 40)) => Jeu Joueur 2`` () =
    let result = ScoreJoueurs (Trente, Quarante) |> joueurGagne Joueur2
    test <@ result = Jeu Joueur2 @>
