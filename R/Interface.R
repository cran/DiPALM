open_log<-function(log_file=NULL)
	{
	if(is.null(log_file))
		return(function(...,sep=' ')
			{
			cat(...,sep=sep)
			if(sep!='\n') cat('\n')
			}
		)
	else
		{
		if(!dir.exists(dirname(log_file)))
			dir.create(dirname(log_file),recursive=TRUE);
		if(!file.create(log_file))
			{
			print('Error opening log file:');
			print(log_file)
			stop(status=1)
			}
		log.file<-file(log_file,'w')
		return(function(...,sep=' ')
			{
			args<-list(...)
			if(length(args)==1)
				{
				#R doesn't short circuit Boolean logic
				#so if we just use & this will error when
				#the list is length 0
				if(is.null(args[[1]]))
					{
					close(log.file)
					return(invisible())
					}
				}
			cat(...,file=log.file,sep=sep)
			if(sep!='\n') cat('\n',file=log.file)
			return(invisible())
			})
		}
	}
re_esc<-function(s){gsub("\\ "," ",gsub("(\\W)", "\\\\\\1",s),fixed=TRUE)} #escapes all non-word characters, source: https://stackoverflow.com/questions/14836754/is-there-an-r-function-to-escape-a-string-for-regex-characters
col_format_sub<-function(col.format='[TREATMENT].[REPLICATE].[TIME]', tr='.*',re='.*',ti='.*',escape=TRUE)
	{
	ret<-msub(
		{if(escape) re_esc('[TIME]') else '[TIME]'},
		ti,
		msub(
			{if(escape) re_esc('[REPLICATE]') else '[REPLICATE]'},
			re,
			msub(
				{if(escape) re_esc('[TREATMENT]') else '[TREATMENT]'},
				tr,
				{if(escape) re_esc(col.format) else col.format},
				fixed=TRUE
				),
			fixed=TRUE
			),
		fixed=TRUE
		)
	return(ret)
	}
#sub but replacement can be a vector
msub<-function(regex,repl,...)
	{
	ret<-sapply(repl, function(r){sub(regex,r,...)})
	if(length(repl)==1)
		{
		return(ret[[1]])
		}
	return(ret)
	}
sampleReplicates<-function(gene.expr,col.format,.log)
	{
	tr.ti<-sub(
		paste0('^',col_format_sub(col.format=col.format,ti='([0-9]+)',tr='(.*)'),'$'),
		'\\1.\\2',
		colnames(gene.expr)
		)
	tr.ti.set<-unique(tr.ti)
	tr.ti.map<-lapply(tr.ti.set, function(tt){as.matrix(nrow=nrow(gene.expr),gene.expr[,tr.ti==tt])})
	names(tr.ti.map)<-tr.ti.set

	.log('Treatment time set: ',names(tr.ti.map))

	num.samples<-min(as.numeric(lapply(tr.ti.map, ncol))) #R can't min a list
	.log('number of samples is',num.samples)
	#sample the data
	tr.ti.map.sampled <- lapply(tr.ti.map, function(x){x[,sample(1:ncol(x), num.samples)]})
	.log('samples taken:',as.character(lapply(tr.ti.map.sampled,colnames)))
	#set colnames because just moving stuff doesn't change anything
	for(i in 1:length(tr.ti.map.sampled))
		{
		colnames(tr.ti.map.sampled[[i]])<-sapply(1:num.samples, function(j){sub('(.*)\\..*\\.(.*)',paste0('\\1.',j,'.\\2'),colnames(tr.ti.map.sampled[[i]])[[j]])})
		}
	gene.expr.sampled<-do.call(cbind, tr.ti.map.sampled)
	rownames(gene.expr.sampled)<-rownames(gene.expr)
	.log('returning sampled gene expression')
	return(gene.expr.sampled)
	}
make_TCs<-function(gene.expr,time_order=NULL,col.format='[TREATMENT].[REPLICATE].[TIME]',.log=open_log())
	{
	#filter out genes with zero variance
	#remove time from colnames with an index
	#split into a list indexed by time
	index<-sub(paste0('^',col_format_sub(col.format=col.format,re='(.*)',tr='(.*)'),'$'), '\\1.\\2',colnames(gene.expr))
	filter<-as.logical(lapply(
		1:nrow(gene.expr),
		function(g)
			{
			all(tapply(1:ncol(gene.expr), INDEX=index,
				function(X)
					{
					sd(gene.expr[g,X])>0
					}
				))
			}
		))
	tapply(1:ncol(gene.expr), INDEX=index,
		function(x) {
			ret<-gene.expr[filter,x]
			colnames(ret)<-sub(col_format_sub(col.format=col.format,ti='(.*)'),'\\1',colnames(ret))

			key<-as.numeric(colnames(ret))
			if(length(time_order)>0)
				{
				key<-match(key,time_order)
					if(any(is.na(key)))
						{
						.log('key',as.numeric(colnames(ret)))
						.log('match(key,time_order))',match(key,time_order))
						TIMES<-unique(sub(col_format_sub(col.format=col.format,ti='(.*)'),'\\1',colnames(gene.expr)))
						stop(paste0("make_TCs: time_order did not specify all time points in data\ntime_order=",time_order,"TIMES=",paste0(TIMES,collapse=', ')))
						}
				}
			ret<-ret[,order(key)]
			return(ret)
		})
	}
##########################################
main<-function(expr.file, threads, wgcna.power, iterations, p.value, sig.ratio, col.format='[TREATMENT].[REPLICATE].[TIME]',ctrl.treatment=c(),file.patt='%d.dipalm.RData',tmpdir='.',logdir='logs',clear.old.files=FALSE)
	{
	args<-ls();

	if(is.null(tmpdir)) tmpdir<-tempdir()
	if(!dir.exists(tmpdir)) dir.create(tmpdir)
	if(!length(grep('%d',file.patt,fixed=TRUE))) file.patt<-paste0('%d',file.patt)

	.log<-open_log(file.path(logdir,paste0("main.",format(Sys.time(),format="%Y.%m.%d.%H.%M"))))
	for(var in args) { .log(var,'=',get(var)); }

	gene.expr<-utils::read.csv(expr.file,row.names=1);
	.log('dim(gene.expr)=',dim(gene.expr));
	#decide which treatment is control
	TREATMENTS<-unique(sub(col_format_sub(col.format=col.format,tr='(.*)'),'\\1',colnames(gene.expr)))
	.log('TREATMENTS =',TREATMENTS);
	{if(is.null(ctrl.treatment))
		{
		dist1<-pwalign::stringDist(c(TREATMENTS[[1]],'CONTROL'))[[1]]
		dist2<-pwalign::stringDist(c(TREATMENTS[[2]],'CONTROL'))[[1]]
		if(dist1<=dist2)
			{
			CONTROL<-TREATMENTS[[1]]
			TEST<-TREATMENTS[[2]]
			}
		else
			{
			CONTROL<-TREATMENTS[[2]]
			TEST<-TREATMENTS[[1]]
			}
		ctrl.treatment<-CONTROL
		.log('Control treatment:',CONTROL)
		.log('Test treatment:',TEST)
		}
	}

	#threads is tuple (number of concurrent dipalm iterations, threads for wgcna in each iteration)
	#default for second entry is 1
	if(length(threads)==1) threads<-c(threads,1);


	files<-file.path(tmpdir,vapply(1:iterations,\(i){gsub("%d",i,file.patt,fixed=TRUE);},''))
	if(clear.old.files) file.remove(files)

	#do the work
	clst<-parallel::makeCluster(threads[[1]]);
	#list of argument lists for all calls
	Dipalm.Args<-lapply(1:iterations, \(i){list(i=i,dipalm.args=list(expr.file=expr.file,wgcna.power=wgcna.power,threads=threads[[1]],col.format=col.format,ctrl.treatment=ctrl.treatment,p.value=p.value),file=files[[i]])})
	.log('Dipalm.Args[[1]]=',paste0(Dipalm.Args[[1]],collapse=', '));
#	parallel::parLapply(cl=clst, X=Dipalm.Args, fun=function(args)
		lapply(Dipalm.Args,function(args) #easier for testing parLapply prevents getting tracebacks
		{
		dipalm.args<-args$dipalm.args
		i<-args$i
		file<-files[[i]]
	#		check if file exists, is loadable and used the same args
		tryCatch({
			loadEnv<-new.env()
			.log("loading file",file);
			load(file,envir=loadEnv);
			.log('done');
			#check whether loaded args and args to use are the same
			#dipalm function records a hash of the gene expression table so we have to add that for comparison
			dipalm.args[['gene.expr']]<-hashr::hash(utils::read.csv(expr.file,row.names=1));
			load.args<-loadEnv[['args']]
			dipalm.names<-sort(names(dipalm.args))
			.log('sorted dipalm names',dipalm.names)
			args.names<-sort(names(load.args))
			.log('sorted arg names',args.names)
			.log('length check:',length(dipalm.args)==length(args.names),length(dipalm.args),length(args.names))
			.log('names check:',dipalm.names%in%args.names)
			.log('value check:',vapply(dipalm.names,\(n){paste0(n,dipalm.args[[n]],if(dipalm.args[[n]]==load.args[[n]]) '==' else '!=', load.args[[n]])}, ''))
			if(length(dipalm.args)!=length(args.names) || any(!dipalm.args%in%load.args)|| any(vapply(dipalm.names,\(n){dipalm.args[[n]]!=load.args[[n]]}, TRUE)) )
				{
				.log('Called with new arguments regenerating',file);
				.log('Previous arguments:',paste0(load.args,collapse=', '));
				.log('New arguments:',paste0(dipalm.args,collapse=', '));
				stop() #run dipalm in error function
				}

			#don't run dipalm use old results
			.log('Keeping old version of',file);
			},
			error=function(e){
				.log('Error, file=',file,'call=',as.character(e$call),'message=',e$message)
				#was just for logging arguments
				dipalm.args[['gene.expr']]<-NULL
				dipalm.logname<-file.path(logdir,paste0("dipalm.",i,".",format(Sys.time(),format="%Y.%m.%d.%H.%M")))
				dipalm.log<-open_log(dipalm.logname)
				#couldn't have .log in arguments log
				dipalm.args[['.log']]<-dipalm.log
				dipalm.ret<-do.call(dipalm,dipalm.args)
				dipalm.log('Writing to file',file)
				do.call(save,as.list(c(names(dipalm.ret),envir=as.environment(dipalm.ret),file=file)))
				dipalm.log('done writing',file)
				dipalm.log(NULL);
			});
		i; #return value for parLapply
		});
	.log('stopping cluster')
	parallel::stopCluster(clst)
	.log('joining outputs')
	ret<-joinOutputs(files,p=p.value,n=as.integer(iterations*sig.ratio),.log=.log)
	.log(NULL);
	return(ret)
	}

dipalm<-function(expr.file,wgcna.power,threads,col.format,ctrl.treatment,p.value,.log=open_log())
	{
	.log('dipalm function')
	gene.expr<-utils::read.csv(expr.file,row.names=1)
	#save args for output validation
	argnames<-ls();
	args<-lapply(argnames,\(s){if(s=='gene.expr')hashr::hash(gene.expr) else get(s)})
	names(args)<-argnames
	.log('arguments:',paste0(vapply(argnames, \(n){ paste0(n,'=',if(n=='gene.expr') hashr::hash(gene.expr) else get(n)); },''),collapse=', '));

	gene.expr<-sampleReplicates(gene.expr,col.format,.log)

	TREATMENTS<-unique(sub(col_format_sub(col.format=col.format,tr='(.*)'),'\\1',colnames(gene.expr)))
	.log('TREATMENTS',TREATMENTS)
	##############
	#WGCNA
	##############
	TCs_List<-make_TCs(gene.expr,col.format=col.format)
	TCs_Mat<-do.call(rbind,TCs_List)
	.log('dim(TCs_Mat) =',dim(TCs_Mat))

	if(threads>=2) enableWGCNAThreads(threads)

	BlockModsAll <- blockwiseModules(datExpr = t(TCs_Mat), power = as.numeric(wgcna.power), networkType = "signed", corType="bicor", TOMType="signed", minModuleSize=100,
	mergeCutHeight=0.30, deepSplit=1, pamRespectsDendro = F, nThreads = threads, verbose=3)
	MEs = (BlockModsAll[[3]])

	##########################################
	#Limma model
	##########################################
	kMEsList<-BuildModMembership(MeMat = MEs, TCsLst = TCs_List)
	Med<-sapply(TCs_List,function(x) apply(x,1,function(y) stats::median(y,na.rm = T)))
	# permute non-merged dataset to estimate the null distribution
	Perm<-lapply(TCs_List,function(x) x[sample(1:nrow(x),nrow(x),replace = T),])
	# calculate kME and kMed scores from permuted data
	kMEsPerm<-BuildModMembership(MeMat = MEs, TCsLst = Perm)
	MedPerm<-sapply(Perm,function(x) apply(x,1,function(y) stats::median(y,na.rm = T)))

	## Construct linear contrasts

	{if(length(TREATMENTS)!=2)
		{
		.log('length(TREATMENTS) =',length(TREATMENTS))
		.log('TREATMENTS=',TREATMENTS)
		.log('This function must be called with two treatments, exiting.')
		.log('Most likely, the column formating of the gene expression table does not match the passed value for `col.format`.');
		.log('col.format=',col.format);
		.log('colnames(gene.expr)=',colnames(gene.expr));
		stop(paste0('dipalm: data must have two treatments\nlength(TREATMENTS)=',length(TREATMENTS),'\nTREATMENTS=',TREATMENTS));
		}
	else
		{
		.log('TREATMENTS =',TREATMENTS)
		if(ctrl.treatment %in% TREATMENTS)
			{
			.log('found control treatment')
			CONTROL<-ctrl.treatment
			.log('CONTROL =',CONTROL)
			TEST<-TREATMENTS[TREATMENTS!=ctrl.treatment][[1]]
			.log('TEST =',TEST)
			}
		else
			{
			.log('Specified control treatment ',ctrl.treatment," not found, exiting.")
			stop(paste0('dipalm: Specified control treatment ',ctrl.treatment,' not found\nTREAMENTS=',TREATMENTS))
			}
		}
	}

		
	repTrt<-unique(sub(col_format_sub(col.format=col.format,tr='(.*)',re='(.*)'),'\\1.\\2',colnames(gene.expr)))
	Treat<-as.factor(c(rep(CONTROL,length(grep(CONTROL,repTrt))), rep(TEST,length(grep(TEST,repTrt)))))
	design<-stats::model.matrix(~0+Treat)
	colnames(design)<-levels(Treat)
	contr<-paste0(TEST,'-',CONTROL)

	## Run the model
	LimmaModskMEs<-lapply(kMEsList, function(x) BuildLimmaLM(dataMat = x, designMat = design, contrastStr = contr))
	LimmaModsMed<-BuildLimmaLM(dataMat = Med, designMat = design, contrastStr = contr)
	# pull out t-scores
	LimmaModskMEs<-do.call(cbind,lapply(LimmaModskMEs,function(x) x$t))
	LimmaModsMed<-LimmaModsMed$t

	# repeat limma tests on permuted data 
	LimmaModskMEsPerm<-lapply(kMEsPerm, function(x) BuildLimmaLM(dataMat = x, designMat = design, contrastStr = contr))
	LimmaModsMedPerm<-BuildLimmaLM(dataMat = MedPerm, designMat = design, contrastStr = contr)
	LimmaModskMEsPerm<-do.call(cbind,lapply(LimmaModskMEsPerm,function(x) x$t))
	LimmaModsMedPerm<-LimmaModsMedPerm$t

	# get the absolute value of the test stats
	TestSumskMEs<-apply(LimmaModskMEs,1, function(x) sum(abs(x),na.rm = T))
	TestSumsMed<-abs(LimmaModsMed[,1])
	# repeat on permuted data
	PermSumskMEs<-apply(LimmaModskMEsPerm,1, function(x) sum(abs(x),na.rm = T))
	PermSumsMed<-abs(LimmaModsMedPerm[,1])
	## Calculate pValues using the individual values from each test sum and permuted test sum with an FDR correction.
	AdjkMEs<-sapply(TestSumskMEs,function(x) AdjustPvalue(tVal = x, tVec = TestSumskMEs, pVec = PermSumskMEs))
	AdjMed<-sapply(TestSumsMed,function(x) AdjustPvalue(tVal = x, tVec = TestSumsMed, pVec = PermSumsMed))

	# pull significant genes
	SigkMEs<-AdjkMEs[which(AdjkMEs<p.value)]
	.log('length(AdjkMEs) =',length(AdjkMEs));
	.log('length(SigkMEs) =',length(SigkMEs))
	SigMed<-AdjMed[which(AdjMed<p.value)]
	.log('length(AdjMed) =',length(AdjMed));
	.log('length(SigMed) =',length(SigMed))

	#clustering distance
	LimmaModskMEsSig<-LimmaModskMEs[names(SigkMEs),]
	patternCor<-cor(t(LimmaModskMEsSig))

	.log('end of dipalm function, returning data')
	return(list(AdjkMEs=AdjkMEs,AdjMed=AdjMed,patternCor=patternCor,args=args))
	}

joinOutputs<-function(files,p,n,.log=open_log())
	{
	loadEnv<-new.env()
	M<-matrix(nrow=0,ncol=0) #AdjkMEs matrix we are building
	Med<-matrix(nrow=0,ncol=0) #AdjMed matrix we are building
	.log('Files to read from:',files)
	for(f in files)
		{
		load(f, envir=loadEnv)
		N<-loadEnv$AdjkMEs #New part to add to M
		Ned<-loadEnv$AdjMed #New part to add to Med
		.log('file',f)
		.log('length(N)=',length(N))
		.log('length(Ned)=',length(Ned))
		#append rows to M if needed (probably only happens at beginning)
		for(r in names(N)[!names(N) %in% rownames(M)])
			{
			M<-rbind(M,rep(1,ncol(M)))
			rownames(M)[[nrow(M)]]<-r
			}
		for(r in names(Ned)[!names(Ned) %in% rownames(Med)])
			{
			Med<-rbind(Med,rep(1,ncol(Med)))
			rownames(Med)[[nrow(Med)]]<-r
			}
		#append rows to N if needed (also appears rare but happens)
		# AdjkMEs
		missing.N.genes<-rownames(M)[!rownames(M) %in% names(N)]
		N<-c(N, rep(1, length(missing.N.genes)))
		names(N)<-c(names(N)[1:(length(N)-length(missing.N.genes))], missing.N.genes)
		.log('length(missing.N.genes)=',length(missing.N.genes));

		M<-cbind(M, N[rownames(M)])
		# AdjMed
		missing.Ned.genes<-rownames(Med)[!rownames(Med) %in% names(Ned)]
		Ned<-c(Ned, rep(1, length(missing.Ned.genes)))
		names(Ned)<-c(names(Ned)[1:(length(Ned)-length(missing.Ned.genes))], missing.Ned.genes)
		.log('length(missing.Ned.genes)=',length(missing.Ned.genes));

		Med<-cbind(Med, Ned[rownames(Med)])
		}
	AdjkMEs<-M
	AdjMed<-Med

	.log('dim(AdjkMEs) =',dim(AdjkMEs),' ')
	.log('dim(AdjMed) =',dim(AdjMed),' ')
	##########################################
	#make sig lists
	##########################################
	sig.genes<-rownames(AdjkMEs)[
		sapply(rownames(AdjkMEs),
			function(g)
				{
				sum(as.numeric(AdjkMEs[g,]<p))>=n
				}
		)]
	sig.genes<-sig.genes[order(sig.genes)]
	.log('length(sig.genes) =',length(sig.genes))
	M<-matrix(c(0),nrow=length(sig.genes),ncol=length(sig.genes))
	counts<-matrix(c(0),nrow=length(sig.genes),ncol=length(sig.genes)) #count freqs to do the average right
	colnames(M)<-sig.genes
	rownames(M)<-sig.genes

	# Pull kMed
	sig.MedGenes<-rownames(AdjMed)[
		sapply(rownames(AdjMed),
			function(g)
				{
				sum(as.numeric(AdjMed[g,]<p))>=n
				}
		)]
	sig.MedGenes<-sig.MedGenes[order(sig.MedGenes)]
	.log('length(sig.MedGenes) =',length(sig.MedGenes))
	Med<-matrix(c(0),nrow=length(sig.MedGenes),ncol=length(sig.MedGenes))
	MedCounts<-matrix(c(0),nrow=length(sig.MedGenes),ncol=length(sig.MedGenes)) #count freqs to do the average right
	colnames(Med)<-sig.MedGenes
	rownames(Med)<-sig.MedGenes

	##########################################
	#make averaged patternCor
	##########################################
	for(f in files)
		{
		load(f,envir=loadEnv)
		N<-loadEnv$patternCor #New part to add to M
		.log('file',f);
		.log('length(N)=',length(N));

		#append rows to N if needed
		missing.N.genes<-rownames(M)[!rownames(M) %in% rownames(N)]
		old.rownames<-rownames(N)
		old.colnames<-colnames(N)
		N<-rbind(N,matrix(NA,ncol=ncol(N),nrow=length(missing.N.genes)))
		N<-cbind(N,matrix(NA,nrow=nrow(N),ncol=length(missing.N.genes)))
		rownames(N)<-c(old.rownames, rownames(M)[match(missing.N.genes,rownames(M))])
		colnames(N)<-c(old.colnames, colnames(M)[match(missing.N.genes,colnames(M))])
		.log('length(missing.N.genes)=',length(missing.N.genes))

		N<-N[rownames(M),colnames(M)]

		na.inds<-is.na(N)
		N[is.na(N)]<-0
		M<-M+N
		N[!na.inds]<-1
		counts<-counts+N
		}

	M<-M/counts
	patternCor<-M
	.log('dim(patternCor) =',dim(patternCor))

	return(list(sig.genes=sig.genes,sig.MedGenes=sig.MedGenes,patternCor=patternCor,AdjkMEs=AdjkMEs,AdjMed=AdjMed))
	}
