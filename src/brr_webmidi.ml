(*----------------------------------------------------------------------------
   Copyright (c) 2020 The brr programmers. All rights reserved.
   Distributed under the ISC license, see terms at the end of the file.
  ---------------------------------------------------------------------------*)

open Brr

module Midi = struct
  module Port = struct
    type t = Jv.t
    include (Jv.Id : Jv.CONV with type t := t)
    let as_target = Ev.target_of_jv

    let open' p = Fut.of_promise ~ok:ignore @@ Jv.call p "open" [||]
    let close p = Fut.of_promise ~ok:ignore @@ Jv.call p "close" [||]

    let id p = Jv.to_jstr @@ Jv.get p "id"
    let manufacturer p = Jv.to_jstr @@ Jv.get p "manufacturer"
    let name p = Jv.to_jstr @@ Jv.get p "name"
    let type' p = Jv.to_jstr @@ Jv.get p "type'"
    let state p = Jv.to_jstr @@ Jv.get p "state"
    let connection p = Jv.to_jstr @@ Jv.get p "connection"

    let sub_of_port subp p =
      let t = type' p in
      if Jstr.equal t subp then p else
      let exp = Jstr.(v "Excepted " + subp + v " port but found: " + t) in
      Jv.throw (Jstr.append exp t)
  end

  module Input = struct
    type t = Jv.t
    include (Jv.Id : Jv.CONV with type t := t)
    let as_target = Ev.target_of_jv
    let as_port = Port.of_jv
    let of_port p = Port.sub_of_port (Jstr.v "input") p
  end

  module Output = struct
    type t = Jv.t
    include (Jv.Id : Jv.CONV with type t := t)
    let as_target = Ev.target_of_jv
    let as_port = Port.of_jv
    let of_port p = Port.sub_of_port (Jstr.v "output") p

    let send ?timestamp_ms o msg =
      let args = match timestamp_ms with
      | None -> [| Tarray.to_jv msg |]
      | Some t -> [| Tarray.to_jv msg; Jv.of_float t |]
      in
      match Jv.call o "send" args with
      | exception Jv.Error e -> Error e | s -> Ok()

    let clear o = ignore @@ Jv.call o "clear" [||]
  end

  module Access = struct
    type t = Jv.t
    include (Jv.Id : Jv.CONV with type t := t)

    let inputs a f acc =
      let it = Jv.It.iterator (Jv.get a "inputs") in
      let f _ v acc = f v acc in
      Jv.It.fold_bindings ~key:Jv.to_jstr ~value:Output.of_jv f it acc

    let outputs a f acc =
      let it = Jv.It.iterator (Jv.get a "outputs") in
      let f _ v acc = f v acc in
      Jv.It.fold_bindings ~key:Jv.to_jstr ~value:Output.of_jv f it acc

    type opts = Jv.t
    let opts ?sysex ?software () =
      let o = Jv.obj [||] in
      Jv.Bool.set_if_some o "sysex" sysex;
      Jv.Bool.set_if_some o "software" software;
      o

    let of_navigator ?opts n =
      let args = match opts with None -> [||] | Some opts -> [| opts |] in
      Fut.of_promise ~ok:of_jv @@
      Jv.call (Navigator.to_jv n) "requestMIDIAccess" args
  end

  module Ev = struct
    module Message = struct
      type t = Jv.t
      let data e = Tarray.of_jv (Jv.get e "data")
    end
    let midimessage = Ev.Type.create (Jstr.v "midimessage")

    module Connection =  struct
      type t = Jv.t
      let port e = Port.of_jv (Jv.get e "port")
    end
    let statechange = Ev.Type.create (Jstr.v "statechange")
  end
end


(*---------------------------------------------------------------------------
   Copyright (c) 2020 The brr programmers

   Permission to use, copy, modify, and/or distribute this software for any
   purpose with or without fee is hereby granted, provided that the above
   copyright notice and this permission notice appear in all copies.

   THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
   WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
   MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
   ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
   WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
   ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
   OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
  ---------------------------------------------------------------------------*)
