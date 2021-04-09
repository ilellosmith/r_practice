# ---------------------------------

# Test suites for server.R

# ---------------------------------
context('unit tests for the server')

testServer(load_data_server, expr = {
  
  # --------------------
  
  # File uploads
  
  # --------------------
  
  # Test load_data catches non .json file endings
  session$setInputs(file_uploads = NULL)
  stopifnot(uploaded_dat() == NULL)
  
  
  # Test load_data catches non-StreamingHistory file names
  
  
  # Test load_data catches absence of required colname(s)
  
  
  # Test load_data dedupes properly
})
