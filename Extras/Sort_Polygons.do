

import delimited "C:\Users\17637\Documents\GitHub\Brazil_GeodesicProjections\Outputs\Data\Polygon_Codes_Orthogonal.csv", clear 

keep pol_id_orthogonal state

duplicates drop pol_id_orthogonal state, force

order state pol_id_orthogonal

gen chunk_number = substr(pol_id_orthogonal,7,1)

gen last_digit = substr(pol_id_orthogonal,-1,.)
gen second_to_last_digit = substr(pol_id_orthogonal,-2,1)
gen third_to_last_digit = substr(pol_id_orthogonal,-3,1)

gen order_in_chunk = ""
replace order_in_chunk = last_digit if second_to_last_digit == "_"
egen two_digit = concat(second_to_last_digit last_digit)
egen three_digit = concat(third_to_last_digit second_to_last_digit last_digit)
replace order_in_chunk = two_digit if third_to_last_digit == "_"
replace order_in_chunk = three_digit if order_in_chunk == ""

drop last_digit second_to_last_digit third_to_last_digit two_digit three_digit

destring chunk_number order_in_chunk, replace

sort chunk_number order_in_chunk
drop chunk_number order_in_chunk

export delimited using "C:\Users\17637\Dropbox\PhD\Research\Presource Curse\Data Directory\Discoveries\Polygon_Codes_Orthogonal_Sorted.csv", replace




import delimited "C:\Users\17637\Documents\GitHub\Brazil_GeodesicProjections\Outputs\Data\Polygon_Codes_Parallel.csv", clear 

keep pol_id_parallel state

duplicates drop pol_id_parallel state, force

order state pol_id_parallel 

gen chunk_number = substr(pol_id_parallel,7,1)

gen last_digit = substr(pol_id_parallel,-1,.)
gen second_to_last_digit = substr(pol_id_parallel,-2,1)
gen third_to_last_digit = substr(pol_id_parallel,-3,1)

gen order_in_chunk = ""
replace order_in_chunk = last_digit if second_to_last_digit == "_"
egen two_digit = concat(second_to_last_digit last_digit)
egen three_digit = concat(third_to_last_digit second_to_last_digit last_digit)
replace order_in_chunk = two_digit if third_to_last_digit == "_"
replace order_in_chunk = three_digit if order_in_chunk == ""

drop last_digit second_to_last_digit third_to_last_digit two_digit three_digit

destring chunk_number order_in_chunk, replace

sort chunk_number order_in_chunk
drop chunk_number order_in_chunk

export delimited using "C:\Users\17637\Dropbox\PhD\Research\Presource Curse\Data Directory\Discoveries\Polygon_Codes_Parallel_Sorted.csv", replace
