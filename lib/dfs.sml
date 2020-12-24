signature DFS = sig
  type edge
  type graph
  structure Node: ORD_KEY
  structure NodeMap: ORD_MAP where type Key.ord_key = Node.ord_key
  structure NodeEdgeSet: ORD_SET where type Key.ord_key = edge * Node.ord_key

  val dfs: graph -> Node.ord_key -> NodeEdgeSet.set
end

functor DfsFn(
  type edge
  type graph
  structure Node: ORD_KEY
  val neighbors: graph -> Node.ord_key -> (edge * Node.ord_key) list
  val nodes: graph -> Node.ord_key list
): DFS = struct

  structure Stack = struct
    type 'a stack = 'a list
    val empty = []
    val push = op::
    val pop = List.getItem
    val isEmpty = List.null
  end

  type edge = edge
  type graph = graph
  structure Node = Node

  datatype color = Discovered | Undiscovered
  structure NodeMap = RedBlackMapFn(Node)
  type kmap = color NodeMap.map
  structure NodeEdgeSet = RedBlackSetFn(struct
    type ord_key = edge * Node.ord_key
    fun compare ((_, x), (_, y)) = Node.compare (x, y)
  end)

  fun dfs g n : NodeEdgeSet.set =
    let
      datatype edge' = Edge of edge | NonEdge
      val init = (List.foldl (fn (n, m) => NodeMap.insert (m, n, Undiscovered)) NodeMap.empty) o nodes
      fun dfs' k s acc =
        case Stack.pop s
          of NONE => acc
           | SOME ((e, x), s') =>
               let
                 val k' = NodeMap.insert (k, x, Discovered)
                 (* skip n in the result *)
                 val acc' = case e
                              of NonEdge => acc
                               | Edge e' => NodeEdgeSet.add (acc, (e', x))
               in
                 case NodeMap.find (k, x)
                   of NONE => dfs' k' s' acc'
                    | SOME Discovered => dfs' k s' acc
                    | SOME Undiscovered =>
                        let
                          val s'' =
                            List.foldl
                            Stack.push
                            s'
                            (List.map (fn (e, n') => (Edge e, n')) (neighbors g x))
                        in
                          dfs' k' s'' acc'
                        end
               end
    in
      dfs' (init g) (Stack.push ((NonEdge, n), Stack.empty)) NodeEdgeSet.empty
    end

end
