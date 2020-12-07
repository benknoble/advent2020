functor DfsFn(
  type edge
  type graph
  structure Node: ORD_KEY
  val neighbors: graph -> Node.ord_key -> (edge * Node.ord_key) list
  val nodes: graph -> Node.ord_key list
) = struct

  structure Stack = struct
    type 'a stack = 'a list
    val empty = []
    val push = op::
    val pop = List.getItem
    val isEmpty = List.null
  end

  (* could just as easily use a Set with member => Discovered, but the NONE case
   * in NodeMap.find below would have to be handled *)
  datatype color = Discovered | Undiscovered
  structure NodeMap = RedBlackMapFn(Node)
  type kmap = color NodeMap.map
  structure NodeSet = RedBlackSetFn(Node)

  fun dfs g n : NodeSet.set =
    let
      val init = (List.foldl (fn (n, m) => NodeMap.insert (m, n, Undiscovered)) NodeMap.empty) o nodes
      fun dfs' k s acc =
        case Stack.pop s
          of NONE => acc
           | SOME (x, s') =>
               let
                 val k' = NodeMap.insert (k, x, Discovered)
                 (* skip n in the result *)
                 val acc' = if Node.compare (x, n) = EQUAL
                            then acc
                            else NodeSet.add (acc, x)
               in
                 case NodeMap.find (k, x)
                   of NONE => dfs' k' s' acc'
                    | SOME Discovered => dfs' k s' acc
                    | SOME Undiscovered =>
                        let val s'' = List.foldl Stack.push s' (List.map #2 (neighbors g x))
                        in dfs' k' s'' acc'
                        end
               end
    in
      dfs' (init g) (Stack.push (n, Stack.empty)) NodeSet.empty
    end

end
