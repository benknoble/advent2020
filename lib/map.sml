signature MAP = ORD_MAP where type Key.ord_key = Point.point

(* probably it's bad if Point.compare (bottom_left, top_right) = GREATER *)
functor HorizontallyTiledMapFn(
  structure M: MAP
  val bottom_left: Point.point
  val top_right: Point.point
): MAP = struct

  exception PointOutOfBounds of Point.point

  fun in_bounds (p: Point.point) =
    (#x bottom_left <= #x p andalso #x p <= #x top_right
    andalso
    #y bottom_left <= #y p andalso #y p <= #y top_right)

  fun mk_in_bounds p =
    if not (in_bounds p) then raise PointOutOfBounds p
    else Point.new (#x p mod #x top_right) (#y p)

  structure Key = M.Key

  type 'a map = 'a M.map

  val empty = M.empty

  val isEmpty = M.isEmpty

  fun singleton (k, v) = M.singleton (mk_in_bounds k, v)

  fun insert (m, k, v) = M.insert (m, (mk_in_bounds k), v)
  fun insert' ((k, v), m) = M.insert' ((mk_in_bounds k, v), m)

  fun insertWith f (m, k, v) = M.insertWith f (m, mk_in_bounds k, v)
  fun insertWithi f (m, k, v) = M.insertWithi f (m, mk_in_bounds k, v)

  fun find (m, k) = M.find (m, mk_in_bounds k)

  fun lookup (m, k) = M.lookup (m, mk_in_bounds k)

  fun inDomain (m, k) =
    M.inDomain (m, mk_in_bounds k)
    handle PointOutOfBounds _ => false

  fun remove (m, k) = M.remove (m, mk_in_bounds k)

  val first = M.first
  val firsti = M.firsti

  val numItems = M.numItems

  val listItems = M.listItems
  val listItemsi = M.listItemsi

  val listKeys = M.listKeys

  val collate = M.collate

  val unionWith = M.unionWith
  val unionWithi = M.unionWithi

  val intersectWith = M.intersectWith
  val intersectWithi = M.intersectWithi

  val mergeWith = M.mergeWith
  val mergeWithi = M.mergeWithi

  val app = M.app
  val appi = M.appi

  val map = M.map
  val mapi = M.mapi

  val foldl = M.foldl
  val foldli = M.foldli
  val foldr = M.foldr
  val foldri = M.foldri

  val filter = M.filter
  val filteri = M.filteri

  val mapPartial = M.mapPartial
  val mapPartiali = M.mapPartiali

  val exists = M.exists
  val existsi = M.existsi
  val all = M.all
  val alli = M.alli

end
