secondaryextinction<-function(originalnet,type=1,r,n)
{
		sufix<-c("specieswithmoreprey","specieswithmorepredators");
		cat(paste("GreatestCluster","NumberClusters","S","L","C","PrimExt","CumSecExt","PrimEdgeRemoved","CumSecEdgeRem",sep=";"),file=paste("./Output_",sufix[type],"_secext_variables_real_",r,"_",nets[n],".dat",sep=""),sep="\n");
		net<-originalnet;
		#identify and print variables
		greatestclu<-ifelse((length(V(net)) > 0),sort(clusters(net)$csize,decreasing=TRUE)[1],0);
		numclu<-clusters(net)$no;
		nvert<-length(V(net));
		nedges<-length(E(net));
		secextinc<-0;
		secedgerem<-0;
		connectance<-nedges/(nvert^2);
		#
		originalbasal<-which((degree(net,mode="in")!=0));#my names is charles, but my mother calls me honey
		noriginalbasal<-length(originalbasal);
		names_originalbasals<-V(net)$name[originalbasal];
		prim_extinction<-0;
		list_sec_extinc<-array(0,nvert);
		list_prim_edgesremoved<-array(0,nvert);
		list_sec_edgesremoved<-array(0,nvert);
		nbasal<-noriginalbasal;
		names_basals<-names_originalbasals;
		names<-V(net)$name
		v_labels<-V(net)$name
		names_species<-V(net)$name
		
		cat(paste(greatestclu,numclu,nvert,nedges,connectance,0,0,0,0,sep=";"),file=paste("./Output_",sufix[type],"_secext_variables_real_",r,"_",nets[n],".dat",sep=""),append=TRUE,sep="\n");			
		while((nvert > 0)){
		  #while there is at least one species in the food web

##### PRIMARY EXTINCTION	
			initial_nedges<-length(E(net));
			#the order of primary extinction - #1--->MorePreyFirst
			max_species<-(which(degree(net,mode="in")==0));
			pos_chosenspecies<-sample(max_species,1);
			name_chosenspecies<-names_species[pos_chosenspecies]
			cat(name_chosenspecies,sep="\n")
			todel_prim<-which(V(net)$name%in%name_chosenspecies);
			
			cat(paste(V(net)$name[todel_prim],sep="\n"),sep="\n",file=paste("./removed_species_by_",sufix[type],"_",nets[n],".dat",sep=""),append=TRUE);
			net<-delete.vertices(net,todel_prim);
			names_species<-names_species[-pos_chosenspecies];
			v_labels<-v_labels[-todel_prim];
			
			prim_extinction<-prim_extinction+1;
			list_prim_edgesremoved[todel_prim]<-initial_nedges-length(E(net)); 
			nvert<-length(V(net));
			
			resultedbasals<-which((degree(net,mode="in")!=0));
			names_resultedbasals<-V(net)$name[resultedbasals];
			newbasals<-which(!names_originalbasals %in% names_resultedbasals);	# which orthotpera doesnt have food anymore
			names_newbasals<-names_originalbasals[newbasals];
			nnewbasals<-length(newbasals);
			cat(paste(nnewbasals," secondary extinctions after plant removal:",names_newbasals), sep="\n")
			secextinc<-0
##### SECONDARY EXTINCTION	
			if(nnewbasals > 0){
			  names_originalbasals <- names_resultedbasals
				todel<-which(V(net)$name%in%names_newbasals);
				nedges<-length(E(net));
			
				net<-delete.vertices(net,todel);
				v_labels<-v_labels[-todel];
				list_sec_extinc[todel_prim]<-list_sec_extinc[todel_prim]+nnewbasals; 
				secextinc<-secextinc+nnewbasals;
				edges_removed<-nedges-length(E(net));
				secedgerem<-secedgerem+edges_removed;
				list_sec_edgesremoved[todel_prim]<-list_sec_edgesremoved[todel_prim]+edges_removed; 

				nvert<-length(V(net));
			}	
			greatestclu<-ifelse((length(V(net)) > 0),sort(clusters(net)$csize,decreasing=TRUE)[1],0);
			numclu<-clusters(net)$no;
			nvert<-length(V(net));
			nedges<-length(E(net));
			connectance<-nedges/(nvert^2);
			cat(paste(greatestclu,numclu,nvert,nedges,connectance,prim_extinction,secextinc,list_prim_edgesremoved[todel_prim],secedgerem,sep=";"),file=paste("./Output_",sufix[type],"_secext_variables_real_",r,"_",nets[n],".dat",sep=""),append=TRUE,sep="\n");			
			cat(paste(name_chosenspecies, secextinc, sep="\t"), file = paste0("keystonepecies_",sufix[type],"_secext_nodes_",nets[n],".dat"), append=TRUE, sep="\n") 
			
		}
		for(i in 1:length(V(originalnet))){
			cat(paste(list_sec_extinc[i]," ",sep=""),file=paste("./Output_",sufix[type],"_secext_nodes_",nets[n],".dat",sep=""),append=TRUE);
			cat(paste(list_sec_edgesremoved[i]," ",sep=""),file=paste("./Output_",sufix[type],"_secext_edges_",nets[n],".dat",sep=""),append=TRUE);
		}
		cat("\n",file=paste("./Output_",sufix[type],"_secext_nodes_",nets[n],".dat",sep=""),append=TRUE);
		cat("\n",file=paste("./Output_",sufix[type],"_secext_edges_",nets[n],".dat",sep=""),append=TRUE);
}


