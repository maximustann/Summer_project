extract <- function(num) {
	#reading files
	userData <<- read.table("userlist.txt", sep  = "\t", header = F)
	latencyData <<- read.table("rtmatrix.txt", sep = "\t", header = F)
	serviceData <<- read.table("wslist.txt", sep = "\t", header = F)

	#Give them names
	names(userData) <<- c("ID", "IP", "Country", "longtitude", "Latitude")
	names(serviceData) <<- c("ID", "WSDL", "Provider", "Country")

	if(num == 3){
		#From first US user, first JP user and first UK user
		#To first Arg user, first Aust user and first Bel user
		#testList <<- userData[userData$Country == "United States", ][1, ]
		#testList <<- rbind(testList, userData[userData$Country == "Japan", ][1, ])
		#testList <<- rbind(testList, userData[userData$Country == "United Kingdom", ][1, ])
		#testService <<- serviceData[serviceData$Country == "Argentina", ][1, ]
		#testService <<- rbind(testService, serviceData[serviceData$Country == "Australia", ][1, ])
		#testService <<- rbind(testService, serviceData[serviceData$Country == "Belgium", ][1, ])

		#change ID "0" to "1"
		#check_zero_label(testList, testService)
		#latencyMatrix <<- latencyData[testList$ID, paste("V", testService$ID, sep = '')]
		#names(latencyMatrix) <<- c("Argentina", "Australia", "Belgium")
		#rownames(latencyMatrix) <<- c("United States", "Japan", "United Kingdom")

		#costMatrix <<- matrix(data = rnorm(3 * 4, mean = 100, sd = 20), nrow = 3, ncol = 4)
		#colnames(costMatrix) <<- c("Argentina", "Australia", "Belgium", "United States")
		#rownames(costMatrix) <<- c("S1", "S2", "S3")


		#test case 1
		latencyVector <- c(5.982, 2.130, 0.710, 5.776, 0, 0.760, 6.984, 2.035, 1.863)
		#test case 2
		#latencyVector <- c(0, 5.776, 6.984, 5.776, 0, 2.035, 6.984, 2.035, 0)
		latencyMatrix <<- matrix(latencyVector, nrow = num, ncol = num)
	}
	else if(num == 5){
		#testList <<- userData[userData$Country == "United States", ][1, ]
		#testList <<- rbind(testList, userData[userData$Country == "Japan", ][1, ])
		#testList <<- rbind(testList, userData[userData$Country == "United Kingdom", ][1, ])
		#testList <<- rbind(testList, userData[userData$Country == "Canada", ][1, ])
		#testList <<- rbind(testList, userData[userData$Country == "Norway", ][1, ])
		#testService <<- serviceData[serviceData$Country == "Argentina", ][1, ]
		#testService <<- rbind(testService, serviceData[serviceData$Country == "Australia", ][1, ])
		#testService <<- rbind(testService, serviceData[serviceData$Country == "Belgium", ][1, ])
		#testService <<- rbind(testService, serviceData[serviceData$Country == "United States", ][1, ])
		#testService <<- rbind(testService, serviceData[serviceData$Country == "Brazil", ][1, ])

		#check_zero_label(testList, testService)

		latencyVector <- c(0, 5.776, 6.984, 1.842, 5.569,
						  5.776, 0, 2.035, 0.799, 0.684,
						  0.684, 2.035, 0, 1.424, 0.519,
						  0.639, 0.68, 0.672, 0.125, 0.812,
						  5.569, 0.684, 0.519, 0.812, 0)
		latencyMatrix <<- matrix(latencyVector, nrow = num, ncol = num)
		#latencyMatrix <<- latencyData[testList$ID, paste("V", testService$ID, sep = '')]
	}
	else if(num == 10){
		testList <<- userData[userData$Country == "United States", ][1, ]
		testList <<- rbind(testList, userData[userData$Country == "Japan", ][1, ])
		testList <<- rbind(testList, userData[userData$Country == "United Kingdom", ][1, ])
		testList <<- rbind(testList, userData[userData$Country == "Canada", ][1, ])
		testList <<- rbind(testList, userData[userData$Country == "Norway", ][1, ])
		testList <<- rbind(testList, userData[userData$Country == "United States", ][2, ])
		testList <<- rbind(testList, userData[userData$Country == "Germany", ][1, ])
		testList <<- rbind(testList, userData[userData$Country == "Italy", ][1, ])
		testList <<- rbind(testList, userData[userData$Country == "Spain", ][2, ])
		testList <<- rbind(testList, userData[userData$Country == "Switzerland", ][1, ])
		testService <<- serviceData[serviceData$Country == "Argentina", ][1, ]
		testService <<- rbind(testService, serviceData[serviceData$Country == "Australia", ][1, ])
		testService <<- rbind(testService, serviceData[serviceData$Country == "Belgium", ][1, ])
		testService <<- rbind(testService, serviceData[serviceData$Country == "United States", ][1, ])
		testService <<- rbind(testService, serviceData[serviceData$Country == "Brazil", ][1, ])
		testService <<- rbind(testService, serviceData[serviceData$Country == "Germany", ][1, ])
		testService <<- rbind(testService, serviceData[serviceData$Country == "France", ][1, ])
		testService <<- rbind(testService, serviceData[serviceData$Country == "Denmark", ][1, ])
		testService <<- rbind(testService, serviceData[serviceData$Country == "Canada", ][1, ])
		testService <<- rbind(testService, serviceData[serviceData$Country == "China", ][1, ])
		check_zero_label(testList, testService)
		latencyMatrix <<- latencyData[testList$ID, paste("V", testService$ID, sep = '')]
	}

	else if(num == 15){
		testList <<- userData[userData$Country == "United States", ][1, ]
		testList <<- rbind(testList, userData[userData$Country == "Japan", ][1, ])
		testList <<- rbind(testList, userData[userData$Country == "United Kingdom", ][1, ])
		testList <<- rbind(testList, userData[userData$Country == "Canada", ][1, ])
		testList <<- rbind(testList, userData[userData$Country == "Norway", ][1, ])

		testList <<- rbind(testList, userData[userData$Country == "United States", ][2, ])
		testList <<- rbind(testList, userData[userData$Country == "Germany", ][1, ])
		testList <<- rbind(testList, userData[userData$Country == "Italy", ][1, ])
		testList <<- rbind(testList, userData[userData$Country == "Spain", ][2, ])
		testList <<- rbind(testList, userData[userData$Country == "Switzerland", ][1, ])

		testList <<- rbind(testList, userData[userData$Country == "United States", ][3, ])
		testList <<- rbind(testList, userData[userData$Country == "Israel", ][1, ])
		testList <<- rbind(testList, userData[userData$Country == "Singapore", ][1, ])
		testList <<- rbind(testList, userData[userData$Country == "Hong Kong", ][1, ])
		testList <<- rbind(testList, userData[userData$Country == "Taiwan", ][1, ])

		testService <<- serviceData[serviceData$Country == "Argentina", ][1, ]
		testService <<- rbind(testService, serviceData[serviceData$Country == "Australia", ][1, ])
		testService <<- rbind(testService, serviceData[serviceData$Country == "Belgium", ][1, ])
		testService <<- rbind(testService, serviceData[serviceData$Country == "United States", ][1, ])
		testService <<- rbind(testService, serviceData[serviceData$Country == "Brazil", ][1, ])

		testService <<- rbind(testService, serviceData[serviceData$Country == "Germany", ][1, ])
		testService <<- rbind(testService, serviceData[serviceData$Country == "France", ][1, ])
		testService <<- rbind(testService, serviceData[serviceData$Country == "Denmark", ][1, ])
		testService <<- rbind(testService, serviceData[serviceData$Country == "Canada", ][1, ])
		testService <<- rbind(testService, serviceData[serviceData$Country == "China", ][1, ])

		testService <<- rbind(testService, serviceData[serviceData$Country == "Taiwan", ][1, ])
		testService <<- rbind(testService, serviceData[serviceData$Country == "Iceland", ][1, ])
		testService <<- rbind(testService, serviceData[serviceData$Country == "United Kingdom", ][1, ])
		testService <<- rbind(testService, serviceData[serviceData$Country == "Singapore", ][1, ])
		testService <<- rbind(testService, serviceData[serviceData$Country == "Slovenia", ][1, ])

		check_zero_label(testList, testService)
		latencyMatrix <<- latencyData[testList$ID, paste("V", testService$ID, sep = '')]
	}
	else if(num == 20){
		testList <<- userData[userData$Country == "United States", ][1, ]
		testList <<- rbind(testList, userData[userData$Country == "Japan", ][1, ])
		testList <<- rbind(testList, userData[userData$Country == "United Kingdom", ][1, ])
		testList <<- rbind(testList, userData[userData$Country == "Canada", ][1, ])
		testList <<- rbind(testList, userData[userData$Country == "Norway", ][1, ])

		testList <<- rbind(testList, userData[userData$Country == "United States", ][2, ])
		testList <<- rbind(testList, userData[userData$Country == "Germany", ][1, ])
		testList <<- rbind(testList, userData[userData$Country == "Italy", ][1, ])
		testList <<- rbind(testList, userData[userData$Country == "Spain", ][2, ])
		testList <<- rbind(testList, userData[userData$Country == "Switzerland", ][1, ])

		testList <<- rbind(testList, userData[userData$Country == "United States", ][3, ])
		testList <<- rbind(testList, userData[userData$Country == "Israel", ][1, ])
		testList <<- rbind(testList, userData[userData$Country == "Singapore", ][1, ])
		testList <<- rbind(testList, userData[userData$Country == "Hong Kong", ][1, ])
		testList <<- rbind(testList, userData[userData$Country == "Taiwan", ][1, ])

		testList <<- rbind(testList, userData[userData$Country == "Greece", ][1, ])
		testList <<- rbind(testList, userData[userData$Country == "Korea, Republic of", ][1, ])
		testList <<- rbind(testList, userData[userData$Country == "Ireland", ][2, ])
		testList <<- rbind(testList, userData[userData$Country == "Portugal", ][1, ])
		testList <<- rbind(testList, userData[userData$Country == "Japan", ][2, ])

		testService <<- serviceData[serviceData$Country == "Argentina", ][1, ]
		testService <<- rbind(testService, serviceData[serviceData$Country == "Australia", ][1, ])
		testService <<- rbind(testService, serviceData[serviceData$Country == "Belgium", ][1, ])
		testService <<- rbind(testService, serviceData[serviceData$Country == "United States", ][1, ])
		testService <<- rbind(testService, serviceData[serviceData$Country == "Brazil", ][1, ])

		testService <<- rbind(testService, serviceData[serviceData$Country == "Germany", ][1, ])
		testService <<- rbind(testService, serviceData[serviceData$Country == "France", ][1, ])
		testService <<- rbind(testService, serviceData[serviceData$Country == "Denmark", ][1, ])
		testService <<- rbind(testService, serviceData[serviceData$Country == "Canada", ][1, ])
		testService <<- rbind(testService, serviceData[serviceData$Country == "China", ][1, ])

		testService <<- rbind(testService, serviceData[serviceData$Country == "Taiwan", ][1, ])
		testService <<- rbind(testService, serviceData[serviceData$Country == "Iceland", ][1, ])
		testService <<- rbind(testService, serviceData[serviceData$Country == "United Kingdom", ][1, ])
		testService <<- rbind(testService, serviceData[serviceData$Country == "Singapore", ][1, ])
		testService <<- rbind(testService, serviceData[serviceData$Country == "Slovenia", ][1, ])

		testService <<- rbind(testService, serviceData[serviceData$Country == "United States", ][2, ])
		testService <<- rbind(testService, serviceData[serviceData$Country == "Denmark", ][2, ])
		testService <<- rbind(testService, serviceData[serviceData$Country == "United Kingdom", ][2, ])
		testService <<- rbind(testService, serviceData[serviceData$Country == "France", ][2, ])
		testService <<- rbind(testService, serviceData[serviceData$Country == "Germany", ][2, ])

		check_zero_label(testList, testService)
		latencyMatrix <<- latencyData[testList$ID, paste("V", testService$ID, sep = '')]
	}
	latencyMatrix
}

check_zero_label <- function(testList, testService){
	if(match(0, testList$ID)){
		testList[testList$ID == 0][1, 1] <<- 1
	}

	if(match(0, testService$ID)){
		testService[testService$ID == 0][1, 1] <<- 1
	}
}
