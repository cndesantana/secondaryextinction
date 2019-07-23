secondaryextinction<-function(originalnet,type,r,n)
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
		originalbasal<-which((degree(net,mode="in")==0));
		noriginalbasal<-length(which((degree(net,mode="in")==0)));
		names_originalbasals<-V(net)$id[originalbasal];
		prim_extinction<-0;
		list_sec_extinc<-array(0,nvert);
		list_prim_edgesremoved<-array(0,nvert);
		list_sec_edgesremoved<-array(0,nvert);
		nbasal<-noriginalbasal;
		names_basals<-names_originalbasals;
		names<-V(net)$id
		if(type == 1){species<-V(net)[order(degree(net,mode="in"),decreasing=TRUE)];}#the order of primary extinction - #1--->MorePreyFirst
		else if(type == 2){species<-V(net)[order(degree(net,mode="out"),decreasing=TRUE)];}#the order of primary extinction - #1--->MorePredatorsFirst
		names_species<-V(net)$id[species]
		
		cat(paste(greatestclu,numclu,nvert,nedges,connectance,0,0,0,0,sep=";"),file=paste("./Output_",sufix[type],"_secext_variables_real_",r,"_",nets[n],".dat",sep=""),append=TRUE,sep="\n");			
		while((nvert > 0)&&(sum(degree(net,mode="in")==0))){#while there is at least one species in the food web and while there is at least one basal species (so it stops if the food web has only a 'loop' in which there are no basal species).

##### PRIMARY EXTINCTION	
			initial_nedges<-length(E(net));
			if(type == 1){max_species<-which(sort(degree(net,mode="in"),decreasing=TRUE)==max(degree(net,mode="in")));}#the order of primary extinction - #1--->MorePreyFirst
			else if(type == 2){max_species<-which(sort(degree(net,mode="out"),decreasing=TRUE)==max(degree(net,mode="out")));}#the order of primary extinction - #1--->MorePreyFirst
			pos_chosenspecies<-sample(max_species,1);
			name_chosenspecies<-names_species[pos_chosenspecies]
			todel_prim<-which(V(net)$id%in%name_chosenspecies);
			
			cat(paste(V(net)$id[todel_prim],sep="\n"),sep="\n",file=paste("./removed_species_by_",sufix[type],"_",nets[n],".dat",sep=""),append=TRUE);
			net<-delete.vertices(net,todel_prim);
			names_species<-names_species[-pos_chosenspecies];
			v_labels<-v_labels[-todel_prim];
			
			prim_extinction<-prim_extinction+1;
			list_prim_edgesremoved[todel_prim]<-initial_nedges-length(E(net)); 
			nvert<-length(V(net));
			resultedbasals<-which((degree(net,mode="in")==0));
			names_resultedbasals<-V(net)$id[resultedbasals];
			newbasals<-which(!names_resultedbasals%in%names_originalbasals);	
			names_newbasals<-names_resultedbasals[newbasals];
			nnewbasals<-length(newbasals);
			
##### SECONDARY EXTINCTION	
			while(nnewbasals > 0){
				todel<-which(V(net)$id%in%names_newbasals);
				nedges<-length(E(net));
			
				net<-delete.vertices(net,todel);
				v_labels<-v_labels[-todel];
				list_sec_extinc[todel_prim]<-list_sec_extinc[todel_prim]+nnewbasals; 
				secextinc<-secextinc+nnewbasals;
				edges_removed<-nedges-length(E(net));
				secedgerem<-secedgerem+edges_removed;
				list_sec_edgesremoved[todel_prim]<-list_sec_edgesremoved[todel_prim]+edges_removed; 

				nvert<-length(V(net));
				position_of_newbasals<-which(names_species%in%names_newbasals);
				names_species<-names_species[-position_of_newbasals];
			
				resultedbasals<-which((degree(net,mode="in")==0));
				names_resultedbasals<-V(net)$id[resultedbasals];
				newbasals<-which(!names_resultedbasals%in%names_originalbasals);	
				names_newbasals<-names_resultedbasals[newbasals];
				nnewbasals<-length(newbasals);
			}	
			 
			greatestclu<-ifelse((length(V(net)) > 0),sort(clusters(net)$csize,decreasing=TRUE)[1],0);
			numclu<-clusters(net)$no;
			nvert<-length(V(net));
			nedges<-length(E(net));
			connectance<-nedges/(nvert^2);
			cat(paste(greatestclu,numclu,nvert,nedges,connectance,prim_extinction,secextinc,list_prim_edgesremoved[todel_prim],secedgerem,sep=";"),file=paste("./Output_",sufix[type],"_secext_variables_real_",r,"_",nets[n],".dat",sep=""),append=TRUE,sep="\n");			
			
		}
		for(i in 1:length(V(originalnet))){
			cat(paste(list_sec_extinc[i]," ",sep=""),file=paste("./Output_",sufix[type],"_secext_nodes_",nets[n],".dat",sep=""),append=TRUE);
			cat(paste(list_sec_edgesremoved[i]," ",sep=""),file=paste("./Output_",sufix[type],"_secext_edges_",nets[n],".dat",sep=""),append=TRUE);
		}
		cat("\n",file=paste("./Output_",sufix[type],"_secext_nodes_",nets[n],".dat",sep=""),append=TRUE);
		cat("\n",file=paste("./Output_",sufix[type],"_secext_edges_",nets[n],".dat",sep=""),append=TRUE);

}


