module Vector = Linalg.Vec.Float
module Matrix = Linalg.Mat.Float

(* Vectors indexed by ints *)
type vector = int Vector.t

(* Matrices indexed by pairs (column, row) *)
type matrix = (int * int) Matrix.t

(* Create a [matrix] overlay over an array of arrays.
   Note how we switch from row major to column major in
   order to comply to [Linalg]'s defaults. *)
let matrix_of_array_array (m : float array array) : matrix =
  let r = Array.length m in
  let c = Array.length m.(0) in
  Matrix.make (Linalg.Tensor.Int.rank_two c r) @@ fun (c, r) -> m.(r).(c)

(* Create a [float array] from a Linalg overlay *)
let vector_to_array (v : vector) : float array =
  let rows = Linalg.Tensor.Int.numel @@ Vector.idim v in
  Array.init rows (fun i -> Vector.get v i)

(* Create a [vector] from a float array *)
let vector_of_array (array : float array) =
  Linalg.Vec.Float.make
    (Linalg.Tensor.Int.rank_one (Array.length array))
    (Array.get array)

(* Construct a sequence out of the elements of a vector *)
let vector_to_seq vec =
  let numel = Linalg.Tensor.Int.numel (Linalg.Vec.Float.idim vec) in
  let rec loop i () =
    if i = numel then Seq.Nil
    else Seq.Cons (Linalg.Vec.Float.get vec i, loop (i + 1))
  in
  loop 0

(* Map a function on the rows of a matrix, yielding a column vector *)
let map_rows f matrix =
  Vector.make (Matrix.rows matrix) (fun r ->
      let row = vector_to_array (Matrix.row matrix r) in
      f row)

(* An empty matrix. *)
let empty_matrix =
  Matrix.make (Linalg.Tensor.Int.rank_two 0 0) @@ fun _ -> assert false
