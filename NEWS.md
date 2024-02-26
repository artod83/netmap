# netmap 0.1.4
Fixed a bug in is_sf where the function would fail if the object had additional
class attributes beside sf and data.frame.

# netmap 0.1.3
Updated package documentation. Fixed a bug in reduce_to_map where the function
would give an error if igraph was installed, but not network.

# netmap 0.1.2
Fixed a bug in ggnetmap in linux where stringsAsFactors would not have a 
default value.

# netmap 0.1.1
Fixed a bug in link_network_map when using igraph networks
without a lookup table.

# netmap 0.1.0
First release of the package.
