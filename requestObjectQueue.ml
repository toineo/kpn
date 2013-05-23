module type SingleType =
  sig 
    type t
  end

module type ObjReqAbsQueue =
  functor (Obj : SingleType) ->
    sig
      type t

      type slot = 
        | Obj of Obj.t
	| Query of (Obj.t -> unit)

      type queue_state =
	(* FIXME : choose better names *)
        | Empty
	| Normal 
	| Request

      val create : unit -> t
      val add : slot -> t -> unit
      val pop : t -> slot
      val get_state : t -> queue_state
    end

module type ObjReqAbsHashtbl =
  sig
    module Queue : ObjReqAbsQueue

    val get_queue : 'a -> 'b
  end

module ObjReqQueue : ObjReqAbsQueue =
  functor (Obj : SingleType) ->
    struct
      type obj = Obj.t
	
      type slot = 
        | Obj of obj
	| Query of (obj -> unit)
	  
      type queue_state =
	(* FIXME : choose better names *)
        | Empty
	| Normal 
	| Request


      type t = slot Queue.t
	
      let create = Queue.create
      let add : (slot -> t -> unit) = Queue.add
      let pop : (t -> slot) = Queue.pop

      let get_state q =
	if Queue.is_empty q then
	  Empty
	    
	else
	  match Queue.peek q with
            | Obj _ -> Normal
	    | Query _ -> Request

    end    
      
module ObjReqHashtbl (Id : SingleType) (Obj : SingleType) =
  struct
    type id = Id.t
    type obj = Obj.t

    module Queue = ObjReqQueue (Obj)

    let objects : (id, Queue.t) Hashtbl.t = Hashtbl.create 577

    let get_queue id =
      try Hashtbl.find objects id 
      with
        | Not_found ->
	  let fifo = Queue.create () in
	  Hashtbl.add objects id fifo;
	  fifo

  end
