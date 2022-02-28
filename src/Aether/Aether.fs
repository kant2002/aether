module Aether

open System

//  Оптика

/// Линза из 'a -> 'b.
type Линза<'a,'b> =
    ('a -> 'b) * ('b -> 'a -> 'a)

/// Призма из 'a -> 'b.
type Призма<'a,'b> =
    ('a -> 'b option) * ('b -> 'a -> 'a)

//  Морфизмы

/// Изоморфизм между 'a <> 'b.
type Изоморфизм<'a,'b> =
    ('a -> 'b) * ('b -> 'a)

/// Эпимрфизм между 'a <> 'b.
type Эпиморфизм<'a,'b> =
    ('a -> 'b option) * ('b -> 'a)

(* Obsolete

    Backwards compatibility shims to make the 2.x-> 3.x transition
    less painful, providing functionally equivalent options where possible.

    To be removed for 9.x releases. *)

/// Частичная линза из a -> b
[<Obsolete ("Используйте Призма<'a, 'b> вместо этого.")>]
type PLens<'a,'b> = Призма<'a, 'b>

/// Полный изоморфизм a <> b
[<Obsolete ("Используйте Изоморфизм<'a, 'b> вместо этого.")>]
type Изо<'a,'b> = Изоморфизм<'a, 'b>

/// Частичный изоморфизм a <> b
[<Obsolete ("Используйте Эпиморфизм<'a, 'b> вместо этого.")>]
type PIso<'a,'b> = Эпиморфизм<'a, 'b>

/// Functions for composing lenses and prisms with other optics, which
/// returns a new lens or prism based on the optic composed. Open `Aether.Operators`
/// to use the infix operator forms of these compositions, which is significantly
/// less verbose.
[<RequireQualifiedAccess>]
module Compose =

    /// Static overloads of the composition function for lenses (>->).
    /// These functions do not generally need to be called directly, but will
    /// be used when calling Compose.optic.
    type Линза =
        | Линза with

        static member (>->) (Линза, (g2, s2): Линза<'b,'c>) =
            fun ((g1, s1): Линза<'a,'b>) ->
                (fun a -> g2 (g1 a)),
                (fun c a -> s1 (s2 c (g1 a)) a) : Линза<'a,'c>

        static member (>->) (Линза, (g2, s2): Призма<'b,'c>) =
            fun ((g1, s1): Линза<'a,'b>) ->
                (fun a -> g2 (g1 a)),
                (fun c a -> s1 (s2 c (g1 a)) a) : Призма<'a,'c>

        static member (>->) (Линза, (f, t): Изоморфизм<'b,'c>) =
            fun ((g, s): Линза<'a,'b>) ->
                (fun a -> f (g a)),
                (fun c a -> s (t c) a) : Линза<'a,'c>

        static member (>->) (Линза, (f, t): Эпиморфизм<'b,'c>) =
            fun ((g, s): Линза<'a,'b>) ->
                (fun a -> f (g a)),
                (fun c a -> s (t c) a) : Призма<'a,'c>

    /// Собирает линзу с оптикой или морфизмом.
    let inline линза л о =
        (Линза >-> о) л

    /// Static overloads of the composition function for prisms (>?>).
    /// These functions do not generally need to be called directly, but will
    /// be used when calling Compose.optic.
    type Призма =
        | Призма with

        static member (>?>) (Призма, (g2, s2): Линза<'b,'c>) =
            fun ((g1, s1): Призма<'a,'b>) ->
                (fun a -> Option.map g2 (g1 a)),
                (fun c a -> Option.map (s2 c) (g1 a) |> function | Some b -> s1 b a
                                                                 | _ -> a) : Призма<'a,'c>

        static member (>?>) (Призма, (g2, s2): Призма<'b,'c>) =
            fun ((g1, s1): Призма<'a,'b>) ->
                (fun a -> Option.bind g2 (g1 a)),
                (fun c a -> Option.map (s2 c) (g1 a) |> function | Some b -> s1 b a
                                                                 | _ -> a) : Призма<'a,'c>

        static member (>?>) (Призма, (f, t): Изоморфизм<'b,'c>) =
            fun ((g, s): Призма<'a,'b>) ->
                (fun a -> Option.map f (g a)),
                (fun c a -> s (t c) a) : Призма<'a,'c>

        static member (>?>) (Призма, (f, t): Эпиморфизм<'b,'c>) =
            fun ((g, s): Призма<'a,'b>) ->
                (fun a -> Option.bind f (g a)),
                (fun c a -> s (t c) a) : Призма<'a,'c>

    /// Собирает призму с оптикой или морфизмом.
    let inline призма p o =
        (Призма >?> o) p

    (* Obsolete

       Backwards compatibility shims to make the 2.x-> 3.x transition
       less painful, providing functionally equivalent options where possible.

       To be removed for 9.x releases. *)

    /// Compose a lens with a lens, giving a lens.
    [<Obsolete ("Use Compose.lens instead.")>]
    let inline lensWithLens l1 l2 =
        линза l1 l2

    /// Compose a lens with a prism, giving a prism.
    [<Obsolete ("Use Compose.lens instead.")>]
    let inline lensWithPrism l1 p1 =
        линза l1 p1

    /// Compose a lens with an isomorphism, giving a lens.
    [<Obsolete ("Use Compose.lens instead.")>]
    let inline lensWithIsomorphism l1 i1 =
        линза l1 i1

    /// Compose a lens with a partial isomorphism, giving a prism.
    [<Obsolete ("Use Compose.lens instead.")>]
    let inline lensWithPartialIsomorphism l1 e1 =
        линза l1 e1

    /// Compose a prism and a lens, giving a prism.
    [<Obsolete ("Use Compose.prism instead.")>]
    let inline prismWithLens p1 l1 =
        призма p1 l1

    /// Compose a prism with a prism, giving a prism.
    [<Obsolete ("Use Compose.prism instead.")>]
    let inline prismWithPrism p1 p2 =
        призма p1 p2

    /// Compose a prism with an isomorphism, giving a prism.
    [<Obsolete ("Use Compose.prism instead.")>]
    let inline prismWithIsomorphism p1 i1 =
        призма p1 i1

    /// Compose a prism with a partial isomorphism, giving a prism.
    [<Obsolete ("Use Compose.prism instead.")>]
    let inline prismWithPartialIsomorphism p1 e1 =
        призма p1 e1

    /// Compose a total lens and a total lens, giving a total lens
    [<Obsolete ("Use Compose.lens instead.")>]
    let totalLensTotalLens (l1: Линза<'a,'b>) (l2: Линза<'b,'c>) : Линза<'a,'c> =
        линза l1 l2

    /// Compose a total lens and a partial lens, giving a partial lens
    [<Obsolete ("Use Compose.lens instead.")>]
    let totalLensPartialLens (l1: Линза<'a,'b>) (p1: Призма<'b,'c>) : Призма<'a,'c> =
        линза l1 p1

    /// Compose a partial lens and a total lens, giving a partial lens
    [<Obsolete ("Use Compose.prism instead.")>]
    let partialLensTotalLens (p1: Призма<'a,'b>) (l1: Линза<'b,'c>) : Призма<'a,'c> =
        призма p1 l1

    /// Compose two partial lenses, giving a partial lens
    [<Obsolete ("Use Compose.prism instead.")>]
    let partialLensPartialLens (p1: Призма<'a,'b>) (p2: Призма<'b,'c>) : Призма<'a,'c> =
        призма p1 p2

    /// Compose a total lens with a total isomorphism, giving a total lens
    [<Obsolete ("Use Compose.lens instead.")>]
    let totalLensTotalIsomorphism (l1: Линза<'a,'b>) (i1: Изоморфизм<'b,'c>) : Линза<'a,'c> =
        линза l1 i1

    /// Compose a total lens with a partial isomorphism, giving a partial lens
    [<Obsolete ("Use Compose.lens instead.")>]
    let totalLensPartialIsomorphism (l1: Линза<'a,'b>) (p1: Эпиморфизм<'b,'c>) : Призма<'a,'c> =
        линза l1 p1

    /// Compose a partial lens with a total isomorphism, giving a partial lens
    [<Obsolete ("Use Compose.prism instead.")>]
    let partialLensTotalIsomorphism (p1: Призма<'a,'b>) (i1: Изоморфизм<'b, 'c>) : Призма<'a,'c> =
        призма p1 i1

    /// Compose a partial lens with a partial isomorphism, giving a partial lens
    [<Obsolete ("Use Compose.prism instead.")>]
    let partialLensPartialIsomorphism (p1: Призма<'a,'b>) (e1: Эпиморфизм<'b,'c>) : Призма<'a,'c> =
        призма p1 e1

/// Functions for using optics to operate on data structures, using the basic optic
/// operations of get, set and map. The functions are overloaded to take either lenses or
/// prisms, with the return type being inferred.
[<RequireQualifiedAccess>]
module Оптика =

    /// Static overloads of the optic get function (^.). These functions do not generally
    /// need to be called directly, but will be used when calling Optic.get.
    type Получить =
        | Получить with

        static member (^.) (Получить, (g, _): Линза<'a,'b>) =
            fun (a: 'a) ->
                g a : 'b

        static member (^.) (Получить, (g, _): Призма<'a,'b>) =
            fun (a: 'a) ->
                g a : 'b option

    /// Get a value using an optic.
    let inline получить оптика target =
        (Получить ^. оптика) target

    /// Static overloads of the optic set function (^=). These functions do
    /// not generally need to be called directly, but will be used when calling
    /// Optic.set.
    type Установить =
        | Установить with

        static member (^=) (Установить, (_, s): Линза<'a,'b>) =
            fun (b: 'b) ->
                s b : 'a -> 'a

        static member (^=) (Установить, (_, s): Призма<'a,'b>) =
            fun (b: 'b) ->
                s b : 'a -> 'a

    /// Установить значение используя оптику.
    let inline установить оптика значение =
        (Установить ^= оптика) значение

    /// Static overloads of the optic map function (%=). These functions do not generally
    /// need to be called directly, but will be used when calling Optic.map.
    type Отображение =
        | Отображение with

        static member (^%) (Отображение, (g, s): Линза<'a,'b>) =
            fun (f: 'b -> 'b) ->
                (fun a -> s (f (g a)) a) : 'a -> 'a

        static member (^%) (Отображение, (g, s): Призма<'a,'b>) =
            fun (f: 'b -> 'b) ->
                (fun a -> Option.map f (g a) |> function | Some b -> s b a
                                                         | _ -> a) : 'a -> 'a

    /// Modify a value using an optic.
    let inline отобразить оптика f =
        (Отображение ^% оптика) f

/// Functions for creating or using lenses.
[<RequireQualifiedAccess>]
module Линза =

    /// Converts an isomorphism into a lens.
    let ofIsomorphism ((f, t): Изоморфизм<'a,'b>) : Линза<'a,'b> =
        f, (fun b _ -> t b)

    (* Obsolete

       Backwards compatibility shims to make the 2.x-> 3.x transition
       less painful, providing functionally equivalent options where possible.

       To be removed for 9.x releases. *)

    /// Get a value using a lens.
    [<Obsolete ("Use Оптика.получить instead.")>]
    let inline получить l =
        Оптика.получить l

    /// Get a value option using a partial lens
    [<Obsolete ("Use Оптика.получить instead.")>]
    let получитьЧастично (p: Призма<'a,'b>) =
        Оптика.получить p

    /// Get a value or a default using a partial lens
    [<Obsolete ("Use Optic.get instead. Compose it with some Option.orElse implementation, not provided by Aether.")>]
    let getPartialOrElse (p: Призма<'a,'b>) =
        fun b -> Оптика.получить p >> (function | Some b -> b | _ -> b)

    /// Set a value using a lens.
    [<Obsolete ("Use Оптика.установить instead.")>]
    let inline установить l =
        Оптика.установить l

    /// Set a value using a partial lens
    [<Obsolete ("Use Optic.set instead.")>]
    let установитьЧастично (p: Призма<'a,'b>) =
        Оптика.установить p

    /// Map a value using a lens.
    [<Obsolete ("Use Оптика.отобразить instead.")>]
    let inline отобразить l =
        Оптика.отобразить l

    /// Modify a value using a partial lens
    [<Obsolete ("Use Оптика.отобразить instead.")>]
    let отобразитьЧастично (p: Призма<'a,'b>) =
        Оптика.отобразить p

/// Functions for creating or using prisms.
[<RequireQualifiedAccess>]
module Prism =

    /// Converts an epimorphism into a prism.
    let ofEpimorphism ((f, t): Эпиморфизм<'a,'b>) : Призма<'a,'b> =
        f, (fun b _ -> t b)

    (* Obsolete

       Backwards compatibility shims to make the 2.x-> 3.x transition
       less painful, providing functionally equivalent options where possible.

       To be removed for 9.x releases. *)

    /// Get a value using a prism.
    [<Obsolete ("Use Optic.get instead.")>]
    let inline получить p =
        Оптика.получить p

    /// Set a value using a prism.
    [<Obsolete ("Use Optic.set instead.")>]
    let inline установить p =
        Оптика.установить p

    /// Map a value using a prism.
    [<Obsolete ("Use Optic.map instead.")>]
    let inline отобразить p =
        Оптика.отобразить p

/// Various optics implemented for common types such as tuples,
/// lists and maps, along with an identity lens.
[<AutoOpen>]
module Optics =

    // Lens for the identity function (does not change the focus of operation).
    let id_ : Линза<'a,'a> =
        (fun x -> x),
        (fun x _ -> x)

    /// Isomorphism between a boxed and unboxed type.
    let box_<'a> : Изоморфизм<obj,'a> =
        unbox<'a>, box

    /// Lens to the first item of a tuple.
    let fst_ : Линза<('a * 'b),'a> =
        fst,
        (fun a t -> a, snd t)

    /// Lens to the second item of a tuple.
    let snd_ : Линза<('a * 'b),'b> =
        snd,
        (fun b t -> fst t, b)

    [<RequireQualifiedAccess>]
    module Array =

        /// Isomorphism to an list.
        let list_ : Изоморфизм<'v[], 'v list> =
            Array.toList,
            Array.ofList

    [<RequireQualifiedAccess>]
    module Choice =

        /// Prism to Choice1Of2.
        let choice1Of2_ : Призма<Choice<_,_>, _> =
            (fun x ->
                match x with
                | Choice1Of2 v -> Some v
                | _ -> None),
            (fun v x ->
                match x with
                | Choice1Of2 _ -> Choice1Of2 v
                | _ -> x)

        /// Prism to Choice2Of2.
        let choice2Of2_ : Призма<Choice<_,_>, _> =
            (fun x ->
                match x with
                | Choice2Of2 v -> Some v
                | _ -> None),
            (fun v x ->
                match x with
                | Choice2Of2 _ -> Choice2Of2 v
                | _ -> x)

    [<RequireQualifiedAccess>]
    module Result =

        /// Prism to Ok.
        let ok_ : Призма<Result<_,_>, _> =
            (fun x ->
                match x with
                | Ok v -> Some v
                | _ -> None),
            (fun v x ->
                match x with
                | Ok _ -> Ok v
                | _ -> x)

        /// Prism to Error.
        let error_ : Призма<Result<_,_>, _> =
            (fun x ->
                match x with
                | Error v -> Some v
                | _ -> None),
            (fun v x ->
                match x with
                | Error _ -> Error v
                | _ -> x)

    [<RequireQualifiedAccess>]
    module List =

        /// Prism to the head of a list.
        let head_ : Призма<'v list, 'v> =
            (function | h :: _ -> Some h
                      | _ -> None),
            (fun v ->
                function | _ :: t -> v :: t
                         | l -> l)

        /// Prism to an indexed position in a list.
        let pos_ (i: int) : Призма<'v list, 'v> =
#if NET35
            (function | l when List.length l > i -> Some (List.nth l i)
#else
            (function | l when List.length l > i -> Some (List.item i l)
#endif
                      | _ -> None),
            (fun v l ->
                List.mapi (fun i' x -> if i = i' then v else x) l)

        /// Prism to the tail of a list.
        let tail_ : Призма<'v list, 'v list> =
            (function | _ :: t -> Some t
                      | _ -> None),
            (fun t ->
                function | h :: _ -> h :: t
                         | [] -> [])

        /// Isomorphism to an array.
        let array_ : Изоморфизм<'v list, 'v[]> =
            List.toArray,
            List.ofArray

    [<RequireQualifiedAccess>]
    module Map =

        /// Prism to a value associated with a key in a map.
        let key_ (k: 'k) : Призма<Map<'k,'v>,'v> =
            Map.tryFind k,
            (fun v x ->
                if Map.containsKey k x then Map.add k v x else x)

        /// Lens to a value option associated with a key in a map.
        let value_ (k: 'k) : Линза<Map<'k,'v>, 'v option> =
            Map.tryFind k,
            (fun v x ->
                match v with
                | Some v -> Map.add k v x
                | _ -> Map.remove k x)

        /// Weak Isomorphism to an array of key-value pairs.
        let array_ : Изоморфизм<Map<'k,'v>, ('k * 'v)[]> =
            Map.toArray,
            Map.ofArray

        /// Weak Isomorphism to a list of key-value pairs.
        let list_ : Изоморфизм<Map<'k,'v>, ('k * 'v) list> =
            Map.toList,
            Map.ofList

    [<RequireQualifiedAccess>]
    module Option =

        /// Prism to the value in an Option.
        let value_ : Призма<'v option, 'v> =
            id,
            (fun v ->
                function | Some _ -> Some v
                         | None -> None)

(* Obsolete

    Backwards compatibility shims to make the 2.x-> 3.x transition
    less painful, providing functionally equivalent options where possible.

    To be removed for 9.x releases. *)

/// Identity lens returning the original item regardless of modifiction
[<Obsolete ("Use Optics.id_ instead.")>]
let id_ : Линза<'a,'a> =
    Optics.id_

/// First item of a tuple giving a total lens
[<Obsolete ("Use Optics.fst_ instead.")>]
let fst_ : Линза<('a * 'b),'a> =
    Optics.fst_

/// Second item of a tuple giving a total lens
[<Obsolete ("Use Optics.snd_ instead.")>]
let snd_ : Линза<('a * 'b),'b> =
    Optics.snd_

/// Head of a list giving a partial lens
[<Obsolete ("Use Optics.List.head_ instead.")>]
let head_ : Призма<'v list, 'v> =
    Optics.List.head_

/// Position of a list giving a partial lens
[<Obsolete ("Use Optics.List.pos_ instead.")>]
let pos_ (i: int) : Призма<'v list, 'v> =
    Optics.List.pos_ i

/// Tail of a list giving a partial lens
[<Obsolete ("Use Optics.List.tail_ instead.")>]
let tail_ : Призма<'v list, 'v list> =
    Optics.List.tail_

/// Key of a map giving a partial lens
[<Obsolete ("Use Optics.Map.key_ instead.")>]
let key_ (k: 'k) : Призма<Map<'k,'v>,'v> =
    Optics.Map.key_ k


/// Optional custom operators for working with optics. Provides more concise
/// syntactic options for working with the functions in the `Compose` and
/// `Optic` modules.
module Операторы =

    /// Compose a lens with an optic or morphism.
    let inline (>->) л о =
        Compose.линза л о

    /// Compose a prism with an optic or morphism.
    let inline (>?>) п о =
        Compose.призма п о

    /// Get a value using an optic.
    let inline (^.) target оптика =
        Оптика.получить оптика target

    /// Set a value using an optic.
    let inline (^=) значение оптика =
        Оптика.установить оптика значение

    /// Modify a value using an optic.
    let inline (^%) f оптика =
        Оптика.отобразить оптика f

    (* Obsolete

       Backwards compatibility shims to make the 2.x-> 3.x transition
       less painful, providing functionally equivalent options where possible.

       To be removed for 9.x releases. *)

    /// Compose a lens and a lens, giving a lens.
    [<Obsolete ("Use >-> instead.")>]
    let inline (>-->) l1 l2 =
        Compose.линза l1 l2

    /// Compose a lens and a prism, giving a prism.
    [<Obsolete ("Use >-> instead.")>]
    let inline (>-?>) l1 l2 =
        Compose.линза l1 l2

    /// Compose a lens with an isomorphism, giving a lens.
    [<Obsolete ("Use >-> instead.")>]
    let inline (<-->) l i =
        Compose.линза l i

    /// Compose a total lens with a partial isomorphism, giving a prism.
    [<Obsolete ("Use >-> instead.")>]
    let inline (<-?>) l i =
        Compose.линза l i

    /// Compose a prism and a lens, giving a prism.
    [<Obsolete ("Use >?> instead.")>]
    let inline (>?->) l1 l2 =
        Compose.призма l1 l2

    /// Compose a prism with a prism, giving a prism.
    [<Obsolete ("Use >?> instead.")>]
    let inline (>??>) l1 l2 =
        Compose.призма l1 l2

    /// Compose a prism with an isomorphism, giving a prism.
    [<Obsolete ("Use >?> instead.")>]
    let inline (<?->) l i =
        Compose.призма l i

    /// Compose a prism with a partial isomorphism, giving a prism.
    [<Obsolete ("Use >?> instead.")>]
    let inline (<??>) l i =
        Compose.призма l i

    /// Get a value using a prism.
    [<Obsolete ("Use ^. instead.")>]
    let inline (^?.) a p =
        Оптика.получить p a

    /// Set a value using a prism.
    [<Obsolete ("Use ^= instead.")>]
    let inline (^?=) b p =
        Оптика.установить p b

    /// Modify a value using a total lens
    [<Obsolete ("Use ^% instead.")>]
    let (^%=) (f: 'b -> 'b) (l: Линза<'a,'b>) : 'a -> 'a =
        Оптика.отобразить l f

    /// Modify a value using a prism.
    [<Obsolete ("Use ^% instead.")>]
    let inline (^?%=) f p =
        Оптика.отобразить p f
