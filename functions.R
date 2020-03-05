
num2id = function(num){
    paste0("num", num)
}

make_num_input = function(num, input){
    id = num2id(num)
    if(is.null(input[[id]])){
        numin = numericInput(id, NULL, value = 4, min = 1, max = 10, width = "80px")
    }else{
        numin = numericInput(id, NULL, value = as.numeric(input[[id]]), min = 1, max = 10, width = "80px")
    }
    fluidRow(column(width = 2, tags$h4(num)), column(width = 8, numin))
}

range2seq = function(rng){
    rng = range(rng)
    seq(rng[1], rng[2])
}

get_value_weights = function(card_range, input){
    vals = range2seq(card_range)
    ids = sapply(vals, num2id)
    count = sapply(ids, function(id){
        req(input[[id]])
    })
    dt = data.table(value = vals, count = count)
    dt
}

generate_deck = function(value_weights, suits){
    values = value_weights[, rep(value, count)]
    deck_dt = as.data.table(expand.grid(values, suits))
    setnames(deck_dt, c("value", "suit"))
    deck_dt[, i := seq(.N)]
    deck_dt
}

generate_hand = function(deck_dt, max_hand, num_players, seed = NA){
    # deck_dt = generate_deck(get_value_weights(input$selCardRange, input), input$selSuits)
    if(!is.na(seed)){
        set.seed(seed)    
    }
    hands_dt = data.table(i = sample(nrow(deck_dt), size = max_hand * num_players))
    hands_dt[, player := ((seq(.N)-1) %% num_players) + 1]
    hands_dt[, hand_order := floor((seq(.N)-1) / num_players) + 1]    
    
    hands_dt = merge(deck_dt, hands_dt, by= "i")
    hands_dt
}

is_legal_suit_count = function(hands_dt, hand_range, min_suits = 3){
    suit_count_dt = rbindlist(lapply(range2seq(hand_range), function(size){
        hands_dt[hand_order <= size, .(hand_size = size, suit_count = length(unique(suit))), .(player)]    
    }))
    suit_count_dt[, legal_suit_count := suit_count >= min_suits]
    suit_count_dt$suit_count = NULL
    suit_count_dt
}

is_contiguous = function(val){
    setequal(val, range2seq(val))
}

is_legal_suit_adjacent = function(hands_dt, hand_range){
    suit_adj_dt = rbindlist(lapply(range2seq(hand_range), function(size){
        hands_dt[hand_order <= size, .(hand_size = size, suit_adj = is_contiguous(value)), .(player, suit)][, .(legal_adjacent = all(suit_adj)), .(player, hand_size)]
    }))
    suit_adj_dt
}

sim_make_hands = function(deck_dt, num_sims, hand_range, num_players, master_seed = 0, bfc = BiocFileCache()){
    rname = digest::digest(list(deck_dt, num_sims, hand_range, num_players, master_seed, sim_make_hands))
    
    ssvRecipes::bfcif(bfc, rname, function(){
        hand_ids = seq(num_sims)
        all_hands_list = lapply(hand_ids, function(hand_id){
            hand_dt = generate_hand(deck_dt, max(hand_range), num_players, master_seed * num_sims + hand_id)    
        })
        names(all_hands_list) = hand_ids
        return(all_hands_list)
    })
    
}
sim_badrito_test_legal = function(all_hands_list, hand_range, bfc = BiocFileCache()){
    rname = digest::digest(list(all_hands_list, hand_range, sim_badrito_test_legal))
    
    ssvRecipes::bfcif(bfc, rname, function(){
        all_legal_suit_list = lapply(all_hands_list, function(hand_dt){
            is_legal_suit_count(hand_dt, hand_range)
        })
        all_lega_adj_list = lapply(all_hands_list, function(hand_dt){
            is_legal_suit_adjacent(hand_dt, hand_range)
        })
        all_lega_adj_dt = rbindlist(all_lega_adj_list, idcol = "hand_id")
        all_legal_suit_dt = rbindlist(all_legal_suit_list, idcol = "hand_id")
        legal_dt = merge(all_lega_adj_dt, all_legal_suit_dt, by = c("hand_id", "player", "hand_size"))
        legal_dt[, legal := legal_adjacent & legal_suit_count]
        legal_dt
    })
    
}

sim_badrito_report_fraction = function(legal_dt){
    legal_dt[, .(fraction_legal = sum(legal)/.N), .(hand_size)]    
}

