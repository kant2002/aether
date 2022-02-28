namespace Aether.Tests

open System
open Aether
open Aether.Операторы
open Aether.Testing.Свойства
open FsCheck
open FsCheck.Xunit
open global.Xunit
open Swensen.Unquote

type СвойствоAttribute = PropertyAttribute
type ФактAttribute = FactAttribute

[<AutoOpen>]
module Data =
    let chars : Изоморфизм<string, char[]> =
        (fun x -> x.ToCharArray ()), (fun x -> String (x))

    let rev : Изоморфизм<char[], char[]> =
        Array.rev, Array.rev

    let times2 = (*) 2
    let maybeInt = function
        | Some x when x % 2 = 0 -> Some (x * 3)
        | Some _ -> None
        | None -> Some 1

module ``Built-in Lenses`` =

    [<Свойство>]
    let ``id_ follows the Lens Laws`` (outer : int) inner dummy =
        Линзы.followsLensLaws id_ outer inner dummy times2

    [<Свойство>]
    let ``fst_ follows the Lens Laws`` (outer : int * int) inner dummy =
        Линзы.followsLensLaws fst_ outer inner dummy times2

    [<Свойство>]
    let ``snd_ follows the Lens Laws`` (outer : int * int) inner dummy =
        Линзы.followsLensLaws snd_ outer inner dummy times2

    [<Свойство>]
    let ``Map.value_ follows the Lens Laws`` key (outer: Map<string,int>) inner dummy =
        Линзы.followsLensLaws (Map.value_ key) outer inner dummy maybeInt

module ``Built-in Prisms`` =
    [<Свойство>]
    let ``Choice.choice1Of2_ follows the Prism Laws`` (outer : Choice<int,int>) inner dummy =
        Призма.followsPrismLaws Choice.choice1Of2_ outer inner dummy times2

    [<Свойство>]
    let ``Choice.choice2Of2_ follows the Prism Laws`` (outer : Choice<int,int>) inner dummy =
        Призма.followsPrismLaws Choice.choice2Of2_ outer inner dummy times2

    [<Свойство>]
    let ``Result.ok_ follows the Prism Laws`` (outer : Result<int,int>) inner dummy =
        Призма.followsPrismLaws Result.ok_ outer inner dummy times2

    [<Свойство>]
    let ``Result.error_ follows the Prism Laws`` (outer : Result<int,int>) inner dummy =
        Призма.followsPrismLaws Result.error_ outer inner dummy times2

    [<Свойство>]
    let ``Option.value_ follows the Prism Laws`` (outer : int option) inner dummy =
        Призма.followsPrismLaws Option.value_ outer inner dummy times2

    [<Свойство>]
    let ``List.head_ follows the Prism Laws`` (outer : int list) inner dummy =
        Призма.followsPrismLaws List.head_ outer inner dummy times2

    [<Свойство>]
    let ``List.tail_ follows the Prism Laws`` (outer : int list) inner dummy =
        Призма.followsPrismLaws List.tail_ outer inner dummy

    [<Свойство>]
    let ``List.pos_ follows the Prism Laws`` (idx : NonNegativeInt) (outer : int list) inner dummy =
        Призма.followsPrismLaws (List.pos_ idx.Get) outer inner dummy times2

    [<Свойство>]
    let ``Map.key_ follows the Prism Laws`` key (outer : Map<string,int>) inner dummy =
        Призма.followsPrismLaws (Map.key_ key) outer inner dummy int

module ``Built-in Isomorphisms`` =
    [<Свойство>]
    let ``Map.list_ follows the Weak Isomorphism Laws`` (outer : Map<string,int>) inner dummy =
        Isomorphism.followsWeakIsomorphismLaws Map.list_ outer inner dummy

    [<Свойство>]
    let ``Map.array_ follows the Weak Isomorphism Laws`` (outer : Map<string,int>) inner dummy =
        Isomorphism.followsWeakIsomorphismLaws Map.array_ outer inner dummy

    [<Свойство>]
    let ``Array.list_ follows the Isomorphism Laws`` (outer : int []) inner dummy =
        Isomorphism.followsIsomorphismLaws Array.list_ outer inner dummy (List.map times2)

    [<Свойство>]
    let ``List.array_ follows the Isomorphism Laws`` (outer : int list) inner dummy =
        Isomorphism.followsIsomorphismLaws List.array_ outer inner dummy (Array.map times2)

    [<Свойство>]
    let ``Choice.choice1Of2_ mapped through Map(toList/ofList) as a partial isomorphism follows the Weak Partial Isomorphism Laws`` (outer : Choice<Map<string,int>,int>) inner dummy =
        Эпиморфизм.followsWeakEpimorphismLaws ((fst Choice.choice1Of2_ >> Option.map Map.toList),(Map.ofList >> Choice1Of2)) outer inner dummy

    [<Свойство>]
    let ``Choice.choice1Of2_ as a partial isomorphism follows the Partial Isomorphism Laws`` (outer : Choice<int,int>) inner dummy =
        Эпиморфизм.followsEpimorphismLaws (fst Choice.choice1Of2_,Choice1Of2) outer inner dummy

type MapExample =
    { MyMap : Map<string,string> }
    with
        static member myMap_ =
            (fun x -> x.MyMap),
            (fun v x -> { x with MyMap = v })

module ``Examplar Usage Tests`` =
    [<Факт>]
    let ``Upserting into a Map using a Lens`` () =
        let example = { MyMap = Map.ofList ["TestKey","TestValue"]}
        let newValue = Линзы.отобразить MapExample.myMap_ (Map.add "TestKey2" "OtherValue") example
        test <@ newValue.MyMap.["TestKey"] = "TestValue" @>
        test <@ newValue.MyMap.["TestKey2"] = "OtherValue" @>

    [<Факт>]
    let ``Updating a value not contained in a Map using a Prism`` () =
        let o = MapExample.myMap_ >-> Map.key_ "TestKey2"
        let example = { MyMap = Map.ofList ["TestKey","TestValue"]}
        let newValue = Призма.установить (MapExample.myMap_ >-> Map.key_ "TestKey2") "OtherValue" example
        test <@ newValue.MyMap.TryFind "TestKey2" = None @>

    [<Факт>]
    let ``Updating a value contained in a Map using a Prism`` () =
        let example = { MyMap = Map.ofList ["TestKey","TestValue"]}
        let newValue = Призма.установить (MapExample.myMap_ >-> Map.key_ "TestKey") "OtherValue" example
        test <@ newValue.MyMap.["TestKey"] = "OtherValue" @>

    [<Факт>]
    let ``Prepending an element onto a List using a Lens`` () =
        test <@ Линзы.отобразить id_ (fun l -> "Head" :: l) ["Tail"] = ["Head"; "Tail"] @>

    [<Факт>]
    let ``Appending a List onto aanother List using a Lens`` () =
        test <@ Линзы.отобразить id_ (fun l -> l @ ["Tail"]) ["Head"] = ["Head"; "Tail"] @>

    [<Факт>]
    let ``Setting the head on an empty List using a Prism`` () =
        test <@ Призма.установить List.head_ "Bad" [] = [] @>

    [<Факт>]
    let ``Setting the head on a non-empty List using a Prism`` () =
        test <@ Призма.установить List.head_ "Good" ["Bad"] = ["Good"] @>

    [<Факт>]
    let ``Setting the tail on an empty List using a Prism`` () =
        test <@ Призма.установить List.tail_ ["Bad"] [] = [] @>

    [<Факт>]
    let ``Setting the tail on a non-empty List using a Prism`` () =
        test <@ Призма.установить List.tail_ ["Tail"] ["Head"; "Bad"; "Value"] = ["Head"; "Tail"] @>

    [<Факт>]
    let ``Setting the tail on a single-element List using a Prism`` () =
        test <@ Призма.установить List.tail_ ["Long"; "Tail"] ["Head"] = ["Head"; "Long"; "Tail"] @>

module ``Basic Lens functions`` =
    [<Факт>]
    let ``Lens.get returns correct values`` () =
        Линзы.получить fst_ ("Good","Bad") =! "Good"

    [<Факт>]
    let ``Lens.set sets value correctly`` () =
        Линзы.установить fst_ "Good" ("Bad",()) =! ("Good",())

    [<Факт>]
    let ``Lens.map modifies values correctly`` () =
        Линзы.отобразить fst_ (fun x -> x + x) ("Good",()) =! ("GoodGood",())

module ``Basic Prism functions`` =
    [<Факт>]
    let ``Prism.get returns correct values for existing values`` () =
        Призма.получить Choice.choice1Of2_ (Choice1Of2 "Good") =! Some "Good"

    [<Факт>]
    let ``Prism.get returns correct value for missing values`` () =
        Призма.получить Choice.choice2Of2_ (Choice1Of2 "Bad") =! None

    [<Факт>]
    let ``Prism.set returns correct values for existing values`` () =
        Призма.установить Choice.choice1Of2_ "Good" (Choice1Of2 "Bad") =! Choice1Of2 "Good"

    [<Факт>]
    let ``Prism.set returns correct value for missing values`` () =
        Призма.установить Choice.choice2Of2_ "Bad" (Choice1Of2 "Good") =! Choice1Of2 "Good"

    [<Факт>]
    let ``Prism.map modifies values correctly for existing values`` () =
        Призма.отобразить Choice.choice1Of2_ (fun x -> x + x) (Choice1Of2 "Good") =! Choice1Of2 "GoodGood"

    [<Факт>]
    let ``Prism.map modifies values correctly for missing values`` () =
        Призма.отобразить Choice.choice2Of2_ (fun x -> x + x) (Choice1Of2 "Good") =! Choice1Of2 "Good"

module ``Isomorphism composition`` =
    module ``over a Lens`` =
        [<Факт>]
        let ``gets value`` () =
            Линзы.получить (fst_ >-> chars) ("Good",()) =! [| 'G'; 'o'; 'o'; 'd' |]

        [<Факт>]
        let ``sets value`` () =
            Линзы.установить (fst_ >-> chars) [| 'G'; 'o'; 'o'; 'd' |] ("Bad",()) =! ("Good",())

        [<Факт>]
        let ``gets value over multiple isomorphisms`` () =
            Линзы.получить (fst_ >-> chars >-> rev) ("dooG",()) =! [| 'G'; 'o'; 'o'; 'd' |]

        [<Факт>]
        let ``sets value over multiple isomorphisms`` () =
            Линзы.установить (fst_ >-> chars >-> rev) [| 'd'; 'o'; 'o'; 'G' |] ("Bad",()) =! ("Good",())

    module ``over a Prism`` =
        [<Факт>]
        let ``gets value when inner exists`` () =
            Призма.получить (Choice.choice1Of2_ >?> chars) (Choice1Of2 "Good") =! Some [| 'G'; 'o'; 'o'; 'd' |]

        [<Факт>]
        let ``gets nothing when inner does not exist`` () =
            Призма.получить (Choice.choice2Of2_ >?> chars) (Choice1Of2 "Bad") =! None

        [<Факт>]
        let ``sets value when inner exists`` () =
            Призма.установить (Choice.choice1Of2_ >?> chars) [| 'G'; 'o'; 'o'; 'd' |] (Choice1Of2 "Bad") =! Choice1Of2 "Good"

        [<Факт>]
        let ``sets nothing when inner does not exist`` () =
            Призма.установить (Choice.choice2Of2_ >?> chars) [| 'B'; 'a'; 'd' |] (Choice1Of2 "Good") =! Choice1Of2 "Good"

        [<Факт>]
        let ``gets value when inner exists over multiple isomorphisms`` () =
            Призма.получить (Choice.choice1Of2_ >?> chars >?> rev) (Choice1Of2 "dooG") =! Some [| 'G'; 'o'; 'o'; 'd' |]

        [<Факт>]
        let ``gets nothing when inner does not exist over multiple isomorphisms`` () =
            Призма.получить (Choice.choice2Of2_ >?> chars >?> rev) (Choice1Of2 "daB") =! None

        [<Факт>]
        let ``sets value when inner exists over multiple isomorphisms`` () =
            Призма.установить (Choice.choice1Of2_ >?> chars >?> rev) [| 'd'; 'o'; 'o'; 'G' |] (Choice1Of2 "Bad") =! Choice1Of2 "Good"

        [<Факт>]
        let ``sets nothing when inner does not exist over multiple isomorphisms`` () =
            Призма.установить (Choice.choice2Of2_ >?> chars >?> rev) [| 'd'; 'a'; 'B' |] (Choice1Of2 "Good") =! Choice1Of2 "Good"
