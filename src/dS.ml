open OpStack

module IntOrder = struct
  type t = int
  let compare = Pervasives.compare
end

module PCMap = Map.Make( IntOrder )
module PCSet = Set.Make( IntOrder )
