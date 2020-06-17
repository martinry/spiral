
spiral <- function(N){
	# Area size
	area <- matrix(1, N, N)
	
	# Find middle for x and y coordinates
	y <- ceiling(N/2)
	x <- ceiling(N/2)
	
	# Direction vector with starting iterations
	d <- c(1, # right
		   1, # down
		   2, # left
		   2) # up
	
	# Sequence of directional indices
	s <- rep(seq(1:4), N**2/4)
	
	i = 1 # iterator from 1:NN
	j = 0 # iterator to keep track of current direction
	
	b = F # Initialize break as false
	
	# Loop until N^2 is reached
	while(i <= N**2){
		# Break if goofed
		if(is.na(s[i]) | b){
			break
		}
		
		if(i > 1) {
			j = j + 1 # Increment dir vector
			
			# Edge case if N is even
			if((N%%2 == 0) & (d[s[j]] + i > N**2)){
				stay <- d[s[j]] - 1 # How many i iterations should we stay in a direction
				b = T
			} else {
				stay <- d[s[j]]
			}
			
			while(stay > 0){
				if(s[j] == 1){
					y = y + 1 # Go right
				} else if(s[j] == 2){
					x = x + 1 # Go down
				} else if(s[j] == 3){
					y = y - 1 # Go left
				} else if (s[j] == 4){
					x = x - 1 # Go up
				}
				
				area[x, y] = i # Set position value to current i iteration
				
				# Increment direction turns for next iteration
				if(stay == 1){
					d[s[j]] = d[s[j]] + 2
				}
				
				stay = stay - 1
				
				if(i >= N**2){
					break
				} else {
					i = i + 1
				}
			}
			# First position	
		} else {
			area[x, y] = i
			i = i + 1
		}
	}
	
	return(area)
}