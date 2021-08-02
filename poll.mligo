type poll_id = string
type poll_option = nat
type votes = ((address * poll_id), poll_option) big_map
type totals = (poll_option, nat) map


type poll_metadata = {
    start_date : timestamp;
    end_date : timestamp;
    num_options : nat;
}

type poll = {
    metadata : poll_metadata;
    totals : totals;
}

let empty_totals_map : (poll_option, nat) map = Map.empty

type polls = (poll_id, poll) big_map

type storage = {
    polls : polls;
    votes : votes;
    administrator : address;
}

type return = (operation list) * storage


type create_poll_arg = {
    poll_id : poll_id;
    poll_metadata : poll_metadata;
}

type vote_arg = {
    poll_id : poll_id;
    vote : poll_option;
}


let check_if_administrator (administrator : address) : unit =
    if Tezos.sender <> administrator then
        ( failwith "error_NOT_AN_ADMINISTRATOR" : unit )
    else
    unit

let create_poll_internal (create_poll_arg, polls : create_poll_arg * polls) : polls =
    if Big_map.mem create_poll_arg.poll_id polls then
        ( failwith "error_POLL_ALREADY_EXIST" : polls )
    else
    let poll = {metadata = create_poll_arg.poll_metadata; totals = empty_totals_map} in
    Big_map.update (create_poll_arg.poll_id) (Some (poll)) polls
    
let update_vote (vote_arg, votes : vote_arg * votes) : poll_option option * votes =
    Big_map.get_and_update ((Tezos.sender, vote_arg.poll_id)) (Some (vote_arg.vote)) votes

let get_poll (poll_id, polls : poll_id * polls) : poll =
    match ( Big_map.find_opt poll_id polls ) with
    | None -> ( failwith "error_NO_SUCH_POLL" : poll )
    | Some p -> p

let update_votes_map (vote_arg, votes : vote_arg * votes) : votes =
    Big_map.update ((Tezos.sender, vote_arg.poll_id)) (Some (vote_arg.vote)) votes

let check_if_poll_started (start_date : timestamp) : unit =
    if Tezos.now < start_date then
        ( failwith "error_POLL_HAS_NOT_STARTED" : unit )
    else unit

let check_if_poll_is_over (end_date : timestamp) : unit =
    if Tezos.now > end_date then
        ( failwith "error_POLL_IS_OVER" : unit )
    else unit

let check_if_vote_option_is_valid (vote, num_options : poll_option * poll_option) : unit =
    if vote > num_options then
        ( failwith "error_VOTE_OPTION_INVALID" : unit )
    else unit

let create_poll ( create_poll_arg, storage : create_poll_arg * storage ) : return =
    let _ = check_if_administrator storage.administrator in
    let polls = create_poll_internal (create_poll_arg, storage.polls) in
    let storage = {storage with polls = polls} in
    (([] : operation list), storage)

let vote ( vote_arg, storage : vote_arg * storage ) : return =
    let poll = get_poll (vote_arg.poll_id, storage.polls) in
    let metadata = poll.metadata in
    let _ = check_if_poll_started metadata.start_date in
    let _ = check_if_poll_is_over metadata.end_date in
    let _ = check_if_vote_option_is_valid (vote_arg.vote, metadata.num_options) in
    let votes = update_votes_map (vote_arg, storage.votes) in
    let storage = {storage with votes = votes} in
    (([] : operation list), storage)   

 type parameter =
    | CreatePoll of create_poll_arg
    | Vote of vote_arg
      
let main (action, s : parameter * storage) : return = 
    (match action with
    | CreatePoll (create_poll_arg) -> create_poll (create_poll_arg, s)
    | Vote (vote_arg) -> vote (vote_arg, s)
    )

let init_storage : storage = {
    polls = ( Big_map.empty : polls );
    votes = ( Big_map.empty : votes );
    administrator = Tezos.sender;
}