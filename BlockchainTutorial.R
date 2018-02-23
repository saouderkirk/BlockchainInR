library(digest)

block_example <- list(index = 1,
                      timestamp = "2018-01-05 17.00 MST",
                      data = "some data",
                      previous_hash = 0,
                      proof = 9,
                      new_hash = NULL
                      )


hash_block <- function(block){
  block$new_hash <- digest(c(block$index,
                             block$timestamp,
                             block$data,
                             block$previous_hash), "sha256" )
  return(block)
}

hashed_example <- hash_block(block_example)


proof_of_work <- function(last_proof){
  proof <- last_proof+ 1
  while (!(proof %% 99 == 0 & proof %% last_proof == 0)) {
    proof <- proof + 1
  }
  return(proof)
}

print(proof_of_work(9))

gen_new_block <- function(previous_block){
  new_proof <- proof_of_work(previous_block$proof)
  
  new_block <- list(index = previous_block$index + 1,
                    timestamp = Sys.time(),
                    data = paste0("this is block", previous_block$index + 1),
                    previous_hash = previous_block$hash,
                    proof = new_proof)
  
  new_block_hashed = hash_block(new_block)
  
  return(new_block_hashed)
}

block_genesis <- list(index = 1,
                      timestamp = Sys.time(),
                      data = "Sega Genesis Block",
                      previous_hash = "0",
                      proof = 1)
blockchain <- list()

## many thanks to https://www.datacamp.com/community/tutorials/blockchain-r
