# (* ::Package:: *)
#
# BeginPackage["Posteriorv03`",{"StatisticalEstimatorsv01`"}];
#
#
# Options[fMinimumDownHill]={"Verbose"->False,Suffix->"","xTolerance"->0.,"fTolerance"->10^-12,"n.restarts"->2,"n.iterations"->10^3};
# fMinimumDownHill::usage"...";
#
# fMinimumPosterior::usage"...";
#
# fMCMC::usage = "CBI[dataTab] returns the bi-weight estimator from Beers et al.";
# fMCMCNoConv::usage = "no convergence criteria";
# fMCMCNoConvExport::usage = "...";
# fRGelmann::usage = "...";
# fGelmanRubin::usage = "...";
# fRGelmannParTab::usage = "...";
#
# fkaMultiBound::usage = "...";
# fkaMultiBoundBinned::usage = "...";
# fKernelBinned2D::usage = "...";
#
# fPriors::usage = "...";
# fPosterior::usage = "...";
# fClearPosterior::usage = "...";
# fChainReading::usage = "...";
# fDownHillPosterior::usage = "...";
#
# fLINMIX::usage = "Linear regression following Kelly (2007).
# fLINMIX[{{x,\[Delta]x,y,\[Delta]y,\[Delta]xy},...}],
#
# Parameters:
# intercept: 1<->\[Alpha]; slope: 2<->\[Beta]; intrinsic scatter: 3<->\*SubscriptBox[\[Sigma], int]
# List of options:
# SharpPriors->{} : ,
# \[Beta]Prior is the prior on the slope \[Beta],
# \[Beta]Prior\[Rule]\"StudentT\" or \"Uniform\",
# xPrior is the prior on the generative function of the independent values
# xPrior\[Rule]\"Uniform\" or \"Gaussian\" or \"GaussianFixed\",
# MalmquistBias: table of threshold values for the dependent variables;
# MalmquistBias-> {} or list of Length[xDataTab] numbers;
# \[Sigma]ParTab\[Rule]{} or list of 5 numbers ,
# nChains\[Rule] number of MCMC chains,
# nBoot-> length of each chain
# \[IndentingNewLine]Malmquist bias: Vikhlinin et al., 2009, Apj, 692, 1033, Eq. A10,\[IndentingNewLine]\!\(\*FractionBox[\(NIntegrate[\*FractionBox[\(1\), \(\*SqrtBox[\(2  \[Pi]\)] \[Sigma]\)] Exp[\(-\*FractionBox[SuperscriptBox[\((y - yTh)\), \(2\)], \(2 \*SuperscriptBox[\(\[Sigma]\), \(2\)]\)]\)] UnitStep[y - yMin] /. {yMin \[Rule] 0.1, yTh \[Rule] 1, \[Sigma] \[Rule] 0.5}, {y, \(-\[Infinity]\), \(+\[Infinity]\)}]\), \(\*FractionBox[\(1\), \(2\)] Erfc[\*FractionBox[\(yMin - yTh\), \(\*SqrtBox[\(2\)]\\\ \[Sigma]\)]] /. {yMin \[Rule] 0.1, yTh \[Rule] 1, \[Sigma] \[Rule] 0.5}\)]\)";
#
#
# fLinearOrthogonal::usage = "Linear regression following Hogg et al. (2010).
#
# fLinearOrthogonal[{{x,\[Delta]x,y,\[Delta]y,\[Delta]xy},...}]
#
# y=\[Alpha]+\[Beta]*x+\*SubscriptBox[\[Sigma], \[Perpendicular]]
#
# Parameters:
# intercept: 1<->\[Alpha]; slope: 2<->\[Beta]; intrinsic scatter: 3<->\*SubscriptBox[\[Sigma], int]
#
# List of options:
# SharpPriors->{},
# \[Beta]Prior is the prior on the slope \[Beta],
# \[Beta]Prior\[Rule]\"StudentT\" or \"Uniform\",
# \[Sigma]ParTab\[Rule]{} or list of 3 numbers ,
# nChains\[Rule] number of MCMC chains,
# nBoot-> length of each chain";
#
#
# fLinearBinomial::usage = "Linear regression with a binomial likelihood.
# fLinearBinomial[{{x,\[Delta]x,y,\[Delta]y,\[Delta]xy},...}]";
#
#
# fPosteriorMCMC::usage = "...";
# fPosteriorMCMCConvergence::usage = "...";
# fPosteriorSampler::usage = "...";
#
#
# (*priors*)
# GaussPrior=1/(Sqrt[2\[Pi]]Par\[Sigma]) Exp[-((Par-Par\[Mu])^2/(2Par\[Sigma]^2))];
# StepPrior=(*xPenaltyv**)(*1/(ParMax-ParMin)*)UnitStep[(ParMin-Par)(Par-ParMax)]+Exp[-200.];
# gtPrior=(*xPenaltyv**)(*1/(ParMax-ParMin)*)UnitStep[Par-ParMin]+Exp[-200.]//N;
# StudentTPrior=(Par\[Nu]/(Par^2+Par\[Nu]))^((1+Par\[Nu])/2)/(Sqrt[Par\[Nu]] Beta[Par\[Nu]/2,1/2])//N;
# (*priors in log space *)
# GaussLogPrior=1/Par 1/(Sqrt[2\[Pi]]ParLog\[Sigma]) Exp[-((Log[Par]-ParLog\[Mu])^2/(2ParLog\[Sigma]^2))];
# StepLogPrior=(*xPenaltyv**)Log[ParMax/ParMin]^-1 1/Par UnitStep[(ParMin-Par)(Par-ParMax)]+Exp[-200.]//N;
# gtLogPrior=(*xPenaltyv**)(*1/(ParMax-ParMin)*)1/Par UnitStep[Par-ParMin]+Exp[-200.]//N;
#
#
#
#
# (* ::Section:: *)
# (*Posteriors and priors*)
#
#
# fPriors[]:=Module[{},
# nPar=Length[ParTab];(*# parameters*)
# IndWithParTab=Table[{i,ParTab[[i]]},{i,nPar}];
#
# ParTabPattern=Map[Pattern[#,_]&,ParTab];
#
# (*parameter name \[Rule] index*)
# PriorsTab[[All,1]]=PriorsTab[[All,1]]/.Flatten[Map[{ParTab[[#]]->#}&,Range[nPar]]];
# PriorsTab=Sort[PriorsTab,#1[[1]]<#2[[1]]&];
#
# SharpPriorsTab[[All,1]]=SharpPriorsTab[[All,1]]/.Flatten[Map[{ParTab[[#]]->#}&,Range[nPar]]];
# SharpPriorsTab=Sort[Union[SharpPriorsTab],#1[[1]]<#2[[1]]&];
#
# (*sharp priors*)
# nParFixed=Length[SharpPriorsTab];
# nParFree=nPar-nParFixed;
# (*
# If[
# SharpPriorsTab!={},
# PriorsTab=Select[PriorsTab,Intersection[{#[[1]]},SharpPriorsTab[[All,1]]]=={}&]
# ];
# *)
#
# SharpPriorsSub=Table[ParTab[[SharpPriorsTab[[IndPar,1]]]]->SharpPriorsTab[[IndPar,3,1,2]],{IndPar,nParFixed}];
# (*SharpPriorsMKSSub=Table[SharpPriorsTab[[IndPar,1]]->ParUnitsTab[[SharpPriorsTab[[IndPar,1]]]]*SharpPriorsTab[[IndPar,3,1,2]],{IndPar,nParFixed}];*)
#
# IndParFixedTab=SharpPriorsTab[[All,1]];
# (*IndParFixedTab=Map[Position[ParTab,#]&,SharpPriorsTab[[All,1]]]//Flatten;*)
# IndParFreeTab=Complement[Range[nPar],IndParFixedTab];
#
# ParFreeTab=ParTab[[IndParFreeTab]];
#
# ParTabPostPriors=ParTab/.SharpPriorsSub/.SharpPriorsSub;
# fParTabAfterSharp[ParTabPattern]:=Evaluate[ParTabPostPriors];
# fParTabAfterSharpOnlyFree[ParTabPattern[[IndParFreeTab]]]:=Evaluate[ParTabPostPriors];
# (*Table[ParTab[[i]]->ParTabValues[[i]],{i,IndParFreeTab}]*)
# ParFixedTab=ParTabPostPriors[[IndParFixedTab]]
#
# ];
#
#
# fPosterior[]:=Module[{},
#
# (*=============== priors   ========================================*)
# fPriorLogicToEvaluate=True;
# Do[
# fPriorLogicToEvaluate=fPriorLogicToEvaluate && If[i[[2]]=="StepPrior" || i[[2]]=="StepLogPrior", ParMin<=Par<=ParMax, ParMin<=Par]/.{Par->ParTab[[i[[1]]]]}/.i[[3]],
# {i,Select[PriorsTab,#[[2]]=="StepPrior" || #[[2]]=="StepLogPrior" || #[[2]]=="gtPrior"&]}];
# fPriorLogic[ParTabPattern]:=Evaluate[fPriorLogicToEvaluate];
#
# fPriorToEvaluate=1.;
# Do[
# fPriorToEvaluate=fPriorToEvaluate*If[i[[2]]=="StepLogPrior" ||i[[2]]=="gtLogPrior", 1/Par,(i[[2]]//ToExpression)]/.{Par->ParTab[[i[[1]]]]}/.i[[3]],
# {i,Select[PriorsTab,#[[2]]!="StepPrior" &&#[[2]]!="gtPrior"&]}];
# fPriorFunction[ParTabPattern]:=Evaluate[fPriorToEvaluate];
#
# fPrior[p_]:=fPriorFunction[p];
#
# (*
# fPriorToEvaluate=Product[PriorsTab[[i,2]]/.{Par->ParTab[[PriorsTab[[i,1]]]]}/.PriorsTab[[i,3]],{i,Length[PriorsTab]}];
# fPrior[ParTabPattern]:=Evaluate[fPriorToEvaluate];
# fPrior[p_]:=fPriorToEvaluate/.Table[ParTab[[i]]->p[[i]],{i,nPar}];
#
# fPosteriorPriorsTmp1[{p:Repeated[_,{nPar}]}]:=fPriorToEvaluate/.Table[ParTab[[i]]->{p}[[i]],{i,nPar}];
# fPrior=Compile[{{p,_Real,1}},Evaluate[fPriorTmp2]];
# *)
#
# (*============\[Equal] likelihood=======================================*)
# fLikelihood[p_]:=If[fPriorLogic[p],Exp[-.5*fChiSquare[p]], 0.0];
#
# (*=============== posterior ========================================*)
# fPosteriorPreSharp[p_]:=fLikelihood[p]*fPrior[p];
#
# (*===============\[Equal] posterior with delta priors  ===============*)
# fPosteriorPostSharp[p_]:=fPosteriorPreSharp[fParTabAfterSharp[p]];
#
# (*============\[Equal] Chi square =======================================*)
# (*=================================================================*)
# (*============\[Equal] Chi square for ML analysis, i.e., only considering boundaries ===========*)
# fChiSquarePreSharpML[p_]:=If[fPriorLogic[p],fChiSquare[p], Exp[200.]];
#
# (*============\[Equal] Chi square as -2*Log[posterior], i.e., without sharp priors  ========*)
# fChiSquarePreSharp[p_]:=fChiSquare[p]-2.0*Log[fPrior[p]]
#
# (*============\[Equal] Chi square with delta priors  ========*)
# (*
# fChiSquarePostSharp[p_]:=Block[{ParSub,ParValuesTab=p},
# ParSub=Table[ParTab[[i]]->p[[i]],{i,nPar}]//Flatten;
# ParValuesTab[[SharpPriorsTab[[All,1]]]]=ParDelta/.SharpPriorsTab[[All,3]]/.ParSub;
# fChiSquarePreSharp[ParValuesTab]//N
# ]
# *)
#
# ];
#
#
# fClearPosterior[]:=Clear[ParTab,ParUnitsTab,ParTagTab,ParStringTab,PriorsTab,SharpPriorsTab,fChiSquare,nPar,IndWithParTab,ParTabPattern,nParFixed,nParFree,SharpPriorsSub,IndParFixedTab,IndParFreeTab,ParFreeTab,ParTabPostPriors,fParTabAfterSharp,fParTabAfterSharpOnlyFree,ParFixedTab,fPriorLogicToEvaluate,fPriorLogicToEvaluate,fPriorLogic,fPriorToEvaluate,fPriorFunction,fPrior,fLikelihood,fPosteriorPreSharp,fPosteriorPostSharp,fChiSquarePreSharpML,fChiSquarePreSharp];
#
#
# (* ::Subsubsection:: *)
# (*Initial points*)
#
#
# (* ::Text:: *)
# (*"xParTab" is a point in the parameter space. "\[Sigma]xParTab" is the list of variances used to run the chains. "nChains" is the # of chains.*)
#
#
# fxParTabIn[xParTab_,\[Sigma]xParTab_,nChains_]:=Module[{ParSub,\[Sigma]FracRandomv,(*x\[Delta]Tmp1,*)xIn,xRandom,x\[Delta]Tab,xParInTab={}},
# xIn=fParTabAfterSharp[xParTab];
# ParSub=Table[ParTab[[i]]->xIn[[i]],{i,nPar}];
# Do[
# If[(iPrior[[2]]=="StepPrior" ||iPrior[[2]]=="StepLogPrior") && (xIn[[iPrior[[1]]]]>(ParMax/.iPrior[[3]]/.ParSub)||xIn [[iPrior[[1]]]]<(ParMin/.iPrior[[3]]/.ParSub)),
# xIn[[iPrior[[1]]]]=0.5(ParMax+ParMin)/.iPrior[[3]]/.ParSub];
# If[(iPrior[[2]]=="gtPrior" ||iPrior[[2]]=="gtLogPrior") && (xIn[[iPrior[[1]]]]<(ParMin/.iPrior[[3]]/.ParSub)),
# xIn[[iPrior[[1]]]]=ParMin+1./.iPrior[[3]]/.ParSub],
# {iPrior,PriorsTab}];
# xIn=fParTabAfterSharp[xIn];
#
# xParInTab=Reap[Do[
# x\[Delta]Tab=Table[If[\[Sigma]xParTab[[i\[Sigma]]]!=0.,RandomReal[NormalDistribution[0.,\[Sigma]xParTab[[i\[Sigma]]]]],0.],{i\[Sigma],Length[\[Sigma]xParTab]}];
# xRandom=xIn+x\[Delta]Tab;
# ParSub=Table[ParTab[[i]]->xRandom[[i]],{i,nPar}];
# Do[
# If[(iPrior[[2]]=="StepPrior" ||iPrior[[2]]=="StepLogPrior") && xRandom[[iPrior[[1]]]]>(ParMax/.iPrior[[3]]/.ParSub),
# x\[Delta]Tab[[iPrior[[1]]]]=RandomReal[{0.,ParMax-xIn[[iPrior[[1]]]]/.iPrior[[3]]/.ParSub}]];
# If[(iPrior[[2]]=="StepPrior" ||iPrior[[2]]=="StepLogPrior"||iPrior[[2]]=="gtPrior" ||iPrior[[2]]=="gtLogPrior") &&
# xRandom[[iPrior[[1]]]]<(ParMin/.iPrior[[3]]/.ParSub),x\[Delta]Tab[[iPrior[[1]]]]=-RandomReal[{0.,xIn[[iPrior[[1]]]]-ParMin/.iPrior[[3]]/.ParSub}]
# ],
# {iPrior,PriorsTab}];
# Sow[fParTabAfterSharp[xIn+x\[Delta]Tab]],
# {iChain,nChains}]][[2,1]];
# Return[xParInTab]
# ];
#
#
# (* ::Subsubsection:: *)
# (*Importing Markov chains*)
#
#
# fChainReading[IndChainTab_,FileChainPattern_]:=Module[{nChainsv,FileChainsTab,ChainsTabTmp1,nToTake,ChainsTab,
# FileSharpPriorsPattern,SharpPriorsMCMCTab,nDroppedv,\[CapitalDelta]nThinv=20,ChainsForConvTab},
# nChainsv=Length[IndChainTab];
#
# FileChainsTab=Table[FileChainPattern<>"_"<>IndChainTab[[i]]<>".dat",{i,nChainsv}];
#
# ChainsTabTmp1=Table[ReadList[FileChainsTab[[i]],Real,RecordLists->True],{i,nChainsv}](*//ToExpression*);
# nToTake=Min[Table[Length[ChainsTabTmp1[[i]]],{i,nChainsv}]];
# nParChains=Length[ChainsTabTmp1[[1,1]]]-1;
# ChainsTab=Table[Take[ChainsTabTmp1[[i]],nToTake],{i,nChainsv}];
#
# (*delta priors*)
# FileSharpPriorsPattern=FileChainPattern<>"_sharp_priors.dat";SharpPriorsMCMCTab=Import[FileSharpPriorsPattern];
# IndParChainsFixedTab=SharpPriorsMCMCTab[[All,1]];
# IndParChainsFreeTab=Complement[Range[1,nParChains],IndParChainsFixedTab];
# nParChainsFixed=nParChains-Length[IndParChainsFreeTab];
# nParChainsFree=nParChains-nParChainsFixed;
#
# SharpPriorsChainsSub=Table[ParTab[[SharpPriorsMCMCTab[[All,1]]]][[IndPar]]->ParUnitsTab[[SharpPriorsMCMCTab[[All,1]]]][[IndPar]]*(SharpPriorsMCMCTab[[All,2]][[IndPar]]//ToExpression),{IndPar,nParChainsFixed}];
#
# ChainsForConvTab=Table[ChainsTab[[i,All,Join[IndParChainsFreeTab,{nParChains+1}]]],{i,nChainsv}];
#
# (*check convergence*)
# nDroppedv=Min[10^3,IntegerPart[0.1*nToTake]];
# DataAllTab=Partition[Table[Drop[ ChainsTab[[i]],nDroppedv],{i,nChainsv}]//Flatten,nParChains+1](*[[All,1;;nParv]]*);
# DataThinnedTab=DataAllTab[[1;;Length[DataAllTab];;\[CapitalDelta]nThinv]];
#
# {Length[DataAllTab],Length[Union[DataAllTab]]/Length[DataAllTab],fRGelmannParTab[ChainsForConvTab,nChainsv,nDroppedv][[4]]}//N
#
# (*MaxFromMCMCTab=Sort[Table[ChainsTab[[i]][[Position[ChainsTab[[i]][[All,nParv+1]],Max[ChainsTab[[i]][[All,nParv+1]]]][[1]]]]//Flatten,{i,nChainsv}],#1[[nParv+1]]>#1[[nParv+1]]&][[1,IndParMCMCFreeTab]];*)
# ];
#
#
# (* ::Subsubsection:: *)
# (*Down-hill minimization*)
#
#
# fMinimumPosterior[OutPutFileName_,xParTabIn_,Opts:OptionsPattern[{Suffix->"", \[Sigma]FracRandom->10.^-1,"priors"->"no" (*yes*),
# "restarts"->"centered"(*"scattered"*),Options[fMinimumDownHill]}//Flatten]]:=Module[
# {fToMinTmp1,fToMinTmp2,xSimpTmp1,xTabIn,xInAll,xInFree,\[Sigma]ParTabIn,xTab,nIntv,\[Sigma]FracRandomv=OptionValue[\[Sigma]FracRandom],
# nRestartv=OptionValue["n.restarts"],nIterations=OptionValue["n.iterations"],Suffixv=OptionValue[Suffix]},
#
# Export[OutPutFileName<>"_sharp_priors_"<>Suffixv<>".dat",Table[{SharpPriorsTab[[i,1]],SharpPriorsTab[[i,3,1,2]]},{i,Length[SharpPriorsTab]}]];
# (*======\[Equal] function to minimize =========*)
# Which[
# OptionValue["priors"]=="no",fToMinTmp2[p_]:=fChiSquarePreSharpML[p],
# OptionValue["priors"]=="yes",fToMinTmp2[p_]:=fChiSquarePreSharp[p]
# ];
#
# If[nParFixed>0,
# fToMinTmp1[p_]:=fToMinTmp2[fParTabAfterSharpOnlyFree[p]],
# fToMinTmp1[p_]:=fToMinTmp2[p]
# ];
#
# xInFree=xParTabIn[[IndParFreeTab]];
#
# Do[
# (*========== initial simplex =============*)
# xInAll=fParTabAfterSharpOnlyFree[xInFree];
# \[Sigma]ParTabIn=Map[If[#!=0.,Abs[\[Sigma]FracRandomv*#],10^-2]&,xInAll];
# Which[
# OptionValue["restarts"]=="centered",xSimpTmp1=Join[{xInAll},fxParTabIn[xInAll,\[Sigma]ParTabIn,nParFree]][[All,IndParFreeTab]],
# OptionValue["restarts"]=="scattered",xSimpTmp1=fxParTabIn[xInAll,\[Sigma]ParTabIn,nParFree+1][[All,IndParFreeTab]]
# ];
# xTabIn=Sort[Map[{#,fToMinTmp1[#]}&,xSimpTmp1],#1[[2]]<#2[[2]]&];
#
# (*======= down hill ============*)
# {xTab,nIntv}=fMinimumDownHill[fToMinTmp1,nParFree,xTabIn,OutPutFileName,FilterRules[{Opts},Options[fMinimumDownHill]]];
# xInFree=xTab[[1,1]];
# If[OptionValue["Verbose"],Print["#",l-1,": ",fParTabAfterSharpOnlyFree[xInFree]," \[Chi]^2 = ",xTab[[1,-1]],", n.iterations= ",nIntv-1]],
# {l,nRestartv+1}];
#
# (*========= export ==============*)
# ParTabBestFitAll=fParTabAfterSharpOnlyFree[xInFree];
# Export[OutPutFileName<>"_best_fit_"<>Suffixv<>".dat",{ParTabBestFitAll}];
# Return[ParTabBestFitAll];
# ];
#
#
# (* ::Subsubsection:: *)
# (*MCMC*)
#
#
# fPosteriorMCMC[xParIn_,\[Sigma]xPar_,nChains_,nBoot_,FileString_]:=Module[
# {PreChainCheck,nSamplesTab,xParInTab,xParInTabTmp1,xParTmp1,p1Tab,p1v,p2v,pTmp1,xPar2,acc,xPar1MCMC,xPar2MCMC,xPar2MCMCFree,xPar1MCMCTab,p1MCMCTab,
# NumKernTab=CharacterRange["1","9"],
# OutPutPreTab,OutPutPostTab,
# FileOutTab},
#
# Export[FileString<>"_sharp_priors.dat",Table[{SharpPriorsTab[[i,1]],SharpPriorsTab[[i,3,1,2]]},{i,Length[SharpPriorsTab]}]];
#
# xParInTabTmp1=fxParTabIn[xParIn,\[Sigma]xPar,nChains];
# xParInTab=Table[fParTabAfterSharp[xParInTabTmp1[[i]]],{i,nChains}];
#
# PreChainCheck=If[Run["ls",FileString<>"_1.dat"]==0,2,1];
# FileOutTab=Table[FileString<>"_"<>NumKernTab[[i]]<>".dat",{i,nChains}];
#
# If[
# PreChainCheck==1,
# OutPutPreTab=Table[{{xParInTab[[j]],fPosteriorPreSharp[xParInTab[[j]]]}//Flatten},{j,nChains}],
# OutPutPreTab=Table[Import[FileOutTab[[j]],"Table" ],{j,nChains}]
# (*OutPutTab=Table[Partition[ReadList[FileOutTab[[j]] ,Real],nPar+1],{j,nChains}]*)
# ];
#
# nSamplesTab=Table[Length[OutPutPreTab[[j]]],{j,nChains}];
#
# xPar1MCMCTab=Table[OutPutPreTab[[i,nSamplesTab[[i]]  ]][[1;;nPar]],{i,nChains}];
# p1MCMCTab=Table[OutPutPreTab[[j,nSamplesTab[[j]],nPar+1  ]],{j,nChains}];
#
# Do[
# p1v=p1MCMCTab[[j]];
# xPar1MCMC=xPar1MCMCTab[[j]];
# OutPutPostTab=Reap[
# Do[
# xPar2MCMC=ParTab/.SharpPriorsSub/.Table[ParTab[[IndPar]]->RandomReal[NormalDistribution[xPar1MCMC[[IndPar]],\[Sigma]xPar[[IndPar]]  ]],{IndPar,IndParFreeTab}];
# p2v=fPosteriorPreSharp[xPar2MCMC];
# acc= RandomReal[];
# If[
# acc<=Min[1.,(p2v/p1v)],
# xPar1MCMC=xPar2MCMC;p1v=p2v
# ];
# Sow[{xPar1MCMC,p1v}//Flatten],
# {IndSample,nSamplesTab[[j]]+1,nBoot}
# ];
# ][[2,1]];
# Export[FileOutTab[[j]],Join[OutPutPreTab[[j]],OutPutPostTab]];
# Clear[OutPutPostTab],
# {j,nChains}];
#
# ];
#
#
# fPosteriorMCMCConvergence[xParIn_,CovMat_,nChains_,nBoot_,FileString_]:=Module[
# {PreChainCheck,nSamplesTab,xParInTab,xParInTabTmp1,xParTmp1,p1Tab,p1v,p2v,pTmp1,xPar2,acc,xPar1MCMC,xPar2MCMC,xPar2MCMCFree,xPar1MCMCTab,p1MCMCTab,
# NumKernTab=CharacterRange["1","9"],
# OutPutPreTab,OutPutPostTab,
# FileOutTab,
# ParTabPostSharpPriors=ParTab/.SharpPriorsSub,
# CovMatFree=CovMat[[IndParFreeTab,IndParFreeTab]],
# \[Sigma]xPar=Diagonal[CovMat]^0.5,
# xPar2FreeMCMC},
#
# Export[FileString<>"_sharp_priors.dat",Table[{SharpPriorsTab[[i,1]],SharpPriorsTab[[i,3,1,2]]},{i,Length[SharpPriorsTab]}]];
#
# xParInTabTmp1=fxParTabIn[xParIn,\[Sigma]xPar,nChains];
# xParInTab=Table[fParTabAfterSharp[xParInTabTmp1[[i]]],{i,nChains}];
#
# PreChainCheck=If[Run["ls",FileString<>"_1.dat"]==0,2,1];
# FileOutTab=Table[FileString<>"_"<>NumKernTab[[i]]<>".dat",{i,nChains}];
#
# If[
# PreChainCheck==1,
# OutPutPreTab=Table[{{xParInTab[[j]],fPosteriorPreSharp[xParInTab[[j]]]}//Flatten},{j,nChains}],
# OutPutPreTab=Table[Import[FileOutTab[[j]],"Table" ],{j,nChains}]
# (*OutPutTab=Table[Partition[ReadList[FileOutTab[[j]] ,Real],nPar+1],{j,nChains}]*)
# ];
#
# nSamplesTab=Table[Length[OutPutPreTab[[j]]],{j,nChains}];
# If[
# Max[nSamplesTab]<nBoot,
# xPar1MCMCTab=Table[OutPutPreTab[[i,nSamplesTab[[i]]  ]][[1;;nPar]],{i,nChains}];
# p1MCMCTab=Table[OutPutPreTab[[j,nSamplesTab[[j]],nPar+1  ]],{j,nChains}];
# Do[
# p1v=p1MCMCTab[[j]];
# xPar1MCMC=xPar1MCMCTab[[j]];
# OutPutPostTab=Reap[
# Do[
# xPar2FreeMCMC=RandomVariate[MultinormalDistribution[xPar1MCMC[[IndParFreeTab]],CovMatFree],1][[1]];
# xPar2MCMC=ParTabPostSharpPriors/.Table[ParFreeTab[[IndPar]]->xPar2FreeMCMC[[IndPar]],{IndPar,nParFree}];
# p2v=fPosteriorPreSharp[xPar2MCMC];
# acc= RandomReal[];
# If[
# acc<=Min[1.,(p2v/p1v)],
# xPar1MCMC=xPar2MCMC;p1v=p2v
# ];
# Sow[{xPar1MCMC,p1v}//Flatten],
# {IndSample,nSamplesTab[[j]]+1,nBoot}
# ];
# ][[2,1]];
# Export[FileOutTab[[j]],Join[OutPutPreTab[[j]],OutPutPostTab]];
# Clear[OutPutPostTab],
# {j,nChains}];
# ]
# ];
#
# Options[fPosteriorSampler]={"n.chains"->4,"n.iter"->2*40*nParFree^2,"initial.points"->"same"(*"scattered"*),"output.file"->""};
# fPosteriorSampler[xParIn_,CovMat_,OptionsPattern[]]:=Block[{nChains,nBoot,FileString,PreChainCheck,nSamplesTab,
# xParInTab,xParInPostTab,p1v,p2v,xPar1MCMC,xPar2MCMC,SampledTab,SamplesTab,FileOutTab,
# ParTabPostSharpPriors=ParTab/.SharpPriorsSub,
# CovMatFree=CovMat[[IndParFreeTab,IndParFreeTab]],
# \[Sigma]xPar=Diagonal[CovMat]^0.5},
#
# nChains=OptionValue["n.chains"];
# nBoot=OptionValue["n.iter"];
# FileString=OptionValue["output.file"];
# (*IndParFreeTab=IntegerPart[IndParFreeTab];*)
#
# Which[OptionValue["initial.points"]=="same",
# xParInTab=ConstantArray[xParIn,nChains],
# OptionValue["initial.points"]=="scattered",
# xParInTab=fxParTabIn[xParIn,\[Sigma]xPar,nChains]
# ];
#
# If[FileString!="",
# Export[FileString<>"_sharp_priors.dat",Table[{SharpPriorsTab[[i,1]],SharpPriorsTab[[i,3,1,2]]},{i,Length[SharpPriorsTab]}]];
# FileOutTab=Map[FileString<>"_"<>#<>".dat"&,CharacterRange["1",nChains//ToString]];
#
# If[
# Run["ls",FileOutTab[[1]]]==0,
# SampledTab=Table[ReadList[FileOutTab[[j]],Real,RecordLists->True],{j,nChains}],
# xParInPostTab=Map[fParTabAfterSharp[#]&,xParInTab];
# SampledTab=Table[{{xParInPostTab[[j]],fPosteriorPreSharp[xParInPostTab[[j]]]}//Flatten},{j,nChains}]
# ];
# nSamplesTab=Table[Length[SampledTab[[j]]],{j,nChains}],
#
# xParInPostTab=Map[fParTabAfterSharp[#]&,xParInTab];
# SampledTab=Table[{{xParInPostTab[[j]],fPosteriorPreSharp[xParInPostTab[[j]]]}//Flatten},{j,nChains}];
# nSamplesTab=ConstantArray[1,nChains];
# ];
#
# SamplesTab=ConstantArray[{},nChains];
# Do[
# p1v=SampledTab[[iChain,-1,nPar+1]];
# xPar1MCMC=xPar2MCMC=SampledTab[[iChain,-1,1;;nPar]];
#
# If[CovMat==DiagonalMatrix[Diagonal[CovMat]],
# SamplesTab[[iChain]]=Reap[
# Do[
# xPar2MCMC[[IndParFreeTab]]=Table[RandomVariate[NormalDistribution[xPar1MCMC[[IndPar]],\[Sigma]xPar[[IndPar]] ]],{IndPar,IndParFreeTab}];
# xPar2MCMC=fParTabAfterSharp[xPar2MCMC];
# p2v=fPosteriorPreSharp[xPar2MCMC];
# If[ RandomReal[]<=p2v/p1v,xPar1MCMC=xPar2MCMC;p1v=p2v];
# Sow[{xPar1MCMC,p1v}//Flatten],
# {IndSample,(*nSamplesTab[[iChain]]+*)2,nBoot}
# ];
# ][[2,1]],
# SamplesTab[[iChain]]=Reap[
# Do[
# xPar2MCMC[[IndParFreeTab]]=RandomVariate[MultinormalDistribution[xPar1MCMC[[IndParFreeTab]],CovMatFree]];
# xPar2MCMC=fParTabAfterSharp[xPar2MCMC];
# p2v=fPosteriorPreSharp[xPar2MCMC];
# If[RandomReal[]<=p2v/p1v,xPar1MCMC=xPar2MCMC;p1v=p2v];Sow[{xPar1MCMC,p1v}//Flatten],
# {IndSample,(*nSamplesTab[[iChain]]+*)2,nBoot}
# ];
# ][[2,1]]
# ];
# Print["chain-",iChain," completed"];
# SamplesTab[[iChain]]=Join[SampledTab[[iChain]],SamplesTab[[iChain]]];
# If[FileString!="",Export[FileOutTab[[iChain]],SamplesTab[[iChain]]];
# Print["chain-",iChain," exported"]],
# {iChain,nChains}];
# Return[SamplesTab]
# ];
#
#
# Options[fEMCEE]={"n.walkers"->5*nPar,"sigma.walkers"->{},"n.iter"->2*40*nParFree^2,"aEMCEE"->2,"output.file"->""};
# fEMCEE[xParIn_,OptionsPattern[]]:=Block[{nWalkers,nBoot,FileString,\[Sigma]xPar,Z,aEMCEE,gEMCEE,XJ,Zgz,ZgzTab,PreChainCheck,
# xParInTab,xParInPostTab,XComplIndTab,XJIndTab,p1Tab,p2,xPar1MCMCTab,xPar2MCMC,SampledTab,SamplesTab,FileOutTab,
# ParTabPostSharpPriors=ParTab/.SharpPriorsSub},
#
# nWalkers=OptionValue["n.walkers"];
# nBoot=OptionValue["n.iter"];
# FileString=OptionValue["output.file"];
# aEMCEE=OptionValue["aEMCEE"];
#
# If[FileString!="",
# Export[FileString<>"_sharp_priors.dat",Table[{SharpPriorsTab[[i,1]],SharpPriorsTab[[i,3,1,2]]},{i,Length[SharpPriorsTab]}]];
# FileOutTab=Map[FileString<>"_"<>#<>".dat"&,CharacterRange["1",nWalkers//ToString]];
# ];
#
# If[FileString==""||(FileString!="" && Run["ls",FileOutTab[[1]]]!=0),
# If[Length[xParIn//Flatten]==nWalkers*nPar,
# xParInTab=xParIn,
# If[(\[Sigma]xPar=OptionValue["sigma.walkers"])=={},\[Sigma]xPar=Table[If[xParIn[[i]]!=0,Abs[10^-1*xParIn[[i]]],10^-2],{i,Length[xParIn]}]];
# xParInTab=Join[{xParIn},fxParTabIn[xParIn,\[Sigma]xPar,nWalkers-1]];
# ];
# xParInPostTab=Map[fParTabAfterSharp[#]&,xParInTab];
# SampledTab=Table[{{xParInPostTab[[j]],fPosteriorPreSharp[xParInPostTab[[j]]]}//Flatten},{j,nWalkers}],
# SampledTab=Table[ReadList[FileOutTab[[j]],Real,RecordLists->True],{j,nWalkers}]
# ];
#
# gEMCEE=ProbabilityDistribution[Sqrt[aEMCEE/Z]/(aEMCEE-1)/2,{Z,1/aEMCEE,aEMCEE}];
# ZgzTab=RandomVariate[gEMCEE,(nBoot-1)*nWalkers];
#
# XComplIndTab=Table[Complement[Range[nWalkers],{iWalker}],{iWalker,nWalkers}];
# XJIndTab=Table[RandomChoice[XComplIndTab[[iWalker]],nBoot-1],{iWalker,nWalkers}];
#
# xPar1MCMCTab=Table[SampledTab[[iWalker,-1,1;;nPar]],{iWalker,nWalkers}];
# p1Tab=Table[SampledTab[[iWalker,-1,-1]],{iWalker,nWalkers}];
#
# SamplesTab=Reap[Do[
# Do[
# XJ=xPar1MCMCTab[[XJIndTab[[iWalker,iBoot]]]];
# Zgz=ZgzTab[[(iBoot-1)*nWalkers+iWalker]];
# xPar2MCMC=fParTabAfterSharp[XJ+Zgz*(xPar1MCMCTab[[iWalker]]-XJ)];
# p2=fPosteriorPreSharp[xPar2MCMC];
# If[RandomReal[]<=Zgz^(nParFree-1)*p2/p1Tab[[iWalker]],xPar1MCMCTab[[iWalker]]=xPar2MCMC;p1Tab[[iWalker]]=p2],
# {iWalker,1,nWalkers}];
# Sow[Table[{xPar1MCMCTab[[iWalker]],p1Tab[[iWalker]]}//Flatten,{iWalker,1,nWalkers}]],
# {iBoot,1,nBoot-1}]
# ][[2,1]];
# SamplesTab=Transpose[SamplesTab];
#
# Do[
# SamplesTab[[iWalker]]=Join[SampledTab[[iWalker]],SamplesTab[[iWalker]]];
# If[FileString!="",Export[FileOutTab[[iWalker]],SamplesTab[[iWalker]]]],
# {iWalker,nWalkers}];
#
# Return[SamplesTab]
# ];
#
#
# (*length of preliminary chains from dunkley+05, tab 2. Rescaling from dunkley+05 and widrow+08*)
# fSamples[xParIn_,\[Sigma]ParIn_,Opts:OptionsPattern[{"n.preliminary.mcmc"-> 2,
# "n.preliminary.iter.min"->40*nParFree^2,"n.preliminary.iter.max"->40*nParFree^2,"sigma.rescaling"->2.4/nParFree^0.5,
# "final.sampling"->"diagonal"(*"covariance"*),Options[fPosteriorSampler]}//Flatten]]:=Block[{nChainsv,nBootv,SigmaRescaling,
# SamplesAllTab,SamplesTab,nMCMCIntermediate,nBootIntMinv,nBootIntMaxv,xMCMCTmp1,\[Sigma]MCMCTabTmp1,CovMatMCMCTmp1,nBootTmp1},
#
# (*===================== sampling ==========================*)
# (*=========================================================*)
# nChainsv=OptionValue["n.chains"];
# nBootv=OptionValue["n.iter"];
# SigmaRescaling=OptionValue["sigma.rescaling"];
#
# (* ==== preliminary chains =======*)
# (*==================================*)
#
# (*=== settings ======*)
# nMCMCIntermediate=OptionValue["n.preliminary.mcmc"] (* # of preliminary chains *);
# nBootIntMinv=OptionValue["n.preliminary.iter.min"](*Floor[1*nBootv]*);(*if nBootIntMinv=nBootIntMaxv all preliminary chains have the same length*)
# nBootIntMaxv=OptionValue["n.preliminary.iter.max"](*Floor[1*nBootv]*);
#
# (*====\[Equal] runs ========*)
# \[Sigma]MCMCTabTmp1=ConstantArray[1.,nPar];
# Do[
# Print["Preliminary chains: #",iMCMC];
# (* ==== initial point ========*)
# If[iMCMC==1,
# xMCMCTmp1=xParIn;
# \[Sigma]MCMCTabTmp1[[IndParFreeTab]]=\[Sigma]ParIn[[IndParFreeTab]],
# xMCMCTmp1=SamplesAllTab[[Position[SamplesAllTab[[All,-1]],Max[SamplesAllTab[[All,-1]]]][[1,1]],1;;-2]];
# \[Sigma]MCMCTabTmp1[[IndParFreeTab]]=SigmaRescaling*SBI[SamplesAllTab[[All,IndParFreeTab]]]
# ];
#
# CovMatMCMCTmp1=DiagonalMatrix[\[Sigma]MCMCTabTmp1^2];
#
# nBootTmp1=Floor[nBootIntMinv+(nBootIntMaxv-nBootIntMinv)*(iMCMC-1)/(nMCMCIntermediate-1)];
#
# SamplesTab=fPosteriorSampler[xMCMCTmp1,CovMatMCMCTmp1,"n.chains"->nChainsv,"n.iter"->nBootTmp1];
#
# SamplesAllTab=SamplesTab[[1]];
# Do[SamplesAllTab=Join[SamplesAllTab,SamplesTab[[Ind]]],{Ind,2,nChainsv}];
#
# Print["Success rate: ",Length[Union[SamplesAllTab]]/Length[SamplesAllTab]//N];
# Print["Gelman-Rubin: ",fGelmanRubin[Table[SamplesTab[[i,All,IndParFreeTab]],{i,nChainsv}]][[3]]//Chop];
#
# ,{iMCMC,nMCMCIntermediate}
# ];
# Clear[nMCMCIntermediate,nBootIntMinv,nBootIntMaxv,nBootTmp1,SamplesTab];
#
# (*======\[Equal] final chains ============*)
# Print["Final chains"];
#
# xMCMCTmp1=SamplesAllTab[[Position[SamplesAllTab[[All,-1]],Max[SamplesAllTab[[All,-1]]]][[1,1]],1;;-2]];
#
# Which[
# OptionValue["final.sampling"]=="diagonal",
# \[Sigma]MCMCTabTmp1[[IndParFreeTab]]=SigmaRescaling*SBI[SamplesAllTab[[All,IndParFreeTab]]];
# CovMatMCMCTmp1=DiagonalMatrix[\[Sigma]MCMCTabTmp1^2],
# OptionValue["final.sampling"]=="covariance",
# CovMatMCMCTmp1=ConstantArray[1.,{nPar,nPar}];
# CovMatMCMCTmp1[[IndParFreeTab,IndParFreeTab]]=SigmaRescaling^2*Covariance[SamplesAllTab[[All,IndParFreeTab]]]//Chop
# ];
#
# SamplesTab=fPosteriorSampler[xMCMCTmp1,CovMatMCMCTmp1,"n.chains"->nChainsv,"n.iter"->nBootv(*,"output.file"\[Rule](FileNameTag=DirDef<>"tmp/"<>TagToWrite<>"_NFW_proj_WL_mcmc")*)];
# Return[SamplesTab];
# ]
#
#
# (* ::Text:: *)
# (*Linear regression, see Kelly (2007)*)
#
#
# Options[fLINMIX]={SharpPriors->{},\[Beta]Prior->"StudentT"(*"Uniform"*),xPrior->"Uniform" (*"Gaussian","GaussianFixed"*),
# MalmquistBias->{},
# \[Sigma]ParTab->{}(*\[Sigma]ParTab*),nChains->4,nBoot->10^3};
#
# fLINMIX[xyDataTab_,OptionsPattern[]]:=Block[{
# MaxParameter=10.^5,ParTab,ParUnitsTab,ParTagTab,PriorsTab,SharpPriorsTabPart1,SharpPriorsTab,
# nData,yFromx,fChiSquareTmp1,fToFit,ParBestFitSub,
# MalmquistBiasTab,MalmquistOn,
# xTabBlock,x\[Mu],x\[Sigma],xMin,xMax,
# yTab,y\[Mu],\[Delta]xTab,\[Delta]x\[Mu],
# xPriorv,\[Alpha]In,\[Beta]In,\[Delta]\[Alpha]In,\[Delta]\[Beta]In,\[Sigma]yIn,nChainsTmp1,nBootv,xCovMat,
# CBITab,SBITab,MeanTab,StandardDeviationTab,
# FileNameTag,fChainRead,\[Sigma]ParTabTmp1,VaryxSub,VaryxNorm},
#
# xPriorv=OptionValue[xPrior];
#
# \[Sigma]ParTabTmp1=OptionValue[\[Sigma]ParTab];
# nChainsTmp1=OptionValue[nChains];
# nBootv=OptionValue[nBoot];
#
# If[OptionValue[MalmquistBias]=={},
# MalmquistOn=0;MalmquistBiasTab=ConstantArray[0.,Length[xyDataTab]],
# MalmquistOn=1;MalmquistBiasTab=OptionValue[MalmquistBias];
# ];
#
# xTabBlock=xyDataTab[[All,1]];
# x\[Mu]=Mean[xTabBlock];
# x\[Sigma]=StandardDeviation[xTabBlock];
# xMin=Min[xTabBlock];
# xMax=Max[xTabBlock];
#
# yTab=xyDataTab[[All,3]];
# y\[Mu]=Mean[yTab];
#
# \[Delta]xTab=xyDataTab[[All,2]];
# \[Delta]x\[Mu]=Mean[\[Delta]xTab];
#
# (*parameters*)
# ParTab={\[Alpha],\[Beta],\[Sigma]y,\[Mu],\[Tau]};
# ParUnitsTab={1.,1.,1.,1,1};
# ParTagTab={"\[Alpha]","\[Beta]","\*SubscriptBox[\[Sigma], y]","\[Mu]","\[Tau]"};
#
# (*priors*)
# PriorsTab={
# {1,"StepPrior",{ParMin->-MaxParameter,ParMax->MaxParameter}},
# {2,"StudentTPrior",{Par\[Nu]->1.}},
# {3,"gtPrior",{ParMin->0.,ParMax->MaxParameter}//N},
# {4,"StepPrior",{ParMin->xMin,ParMax->xMax}//N},
# {5,"StepPrior",{ParMin->x\[Sigma]/MaxParameter,ParMax->xMax-xMin}//N}
# };
#
# If[OptionValue[\[Beta]Prior]=="Uniform",PriorsTab[[2]]={2,"StepPrior",{ParMin->-MaxParameter,ParMax->MaxParameter}}];
#
# SharpPriorsTab=OptionValue[SharpPriors];
#
# Which[
# xPriorv=="Uniform",
# SharpPriorsTab=Join[{
# {4,"SharpPrior",{ParDelta->-99}},{5,"SharpPrior",{ParDelta->-99}}},SharpPriorsTab],
# xPriorv=="GaussianFixed",SharpPriorsTab=Join[{{4,"SharpPrior",{ParDelta->x\[Mu]}},{5,"SharpPrior",{ParDelta->x\[Sigma]}}},SharpPriorsTab](*,
# xPriorv=="Gaussian",
# SharpPriorsTab=SharpPriorsTab
# *)
# ];
#
# fPriors[];
#
# nData=Length[xyDataTab];
# yFromx=\[Alpha]+\[Beta]*x;
# VaryxSub=Varyx->\[Beta]^2 \[Tau]^2+\[Sigma]y^2+\[Delta]y^2-(\[Beta]*\[Tau]^2+\[Delta]xy)^2/(\[Tau]^2+\[Delta]x^2)(*Eq .23, typos corrected, see statiscial notes*);
#
# \[Beta]In=Covariance[xTabBlock,yTab]/x\[Sigma]^2;
# \[Alpha]In=y\[Mu]-\[Beta]In*x\[Mu];\[Sigma]yIn=\[Sqrt]((nData-1)/(nData-2.)*(StandardDeviation[yTab ]^2-\[Beta]In*Covariance[xTabBlock,yTab]));
# (*\[Sigma]yIn=Sqrt[1./(nData-2.)*Total[(yTab-(\[Alpha]In+\[Beta]In*xTabBlock))^2]];*)
# \[Delta]\[Alpha]In=\[Sigma]yIn/nData Sqrt[1.+x\[Mu]^2/((nData-1.)/nData x\[Sigma]^2)];
# \[Delta]\[Beta]In=\[Sigma]yIn/(Sqrt[(nData-1.)/nData]*x\[Sigma]);
#
# VaryxNorm=Varyx/.VaryxSub/.{x->x\[Mu],y->y\[Mu],\[Delta]x->\[Delta]x\[Mu],\[Delta]y->Mean[xyDataTab[[All,4]]],\[Delta]xy->Mean[xyDataTab[[All,5]]]}/.{\[Beta]->\[Beta]In,\[Sigma]y->\[Sigma]yIn,\[Tau]->x\[Sigma]};
#
# fChiSquareTmp1=If[
# xPriorv=="Uniform" (*Eq. 24, I also included the correlation*),
# Sum[(y-yFromx)^2/Varyx+ Log[2.\[Pi]*Varyx/(VaryxNorm/.\[Tau]->MaxParameter)]
# +MalmquistOn*(2.Log[0.5Erfc[(yMin-Eyx)/(Sqrt[2.]*Varyx^0.5)]](*-2.Log[UnitStep[y-yMin]+Exp[-200.]]*))/.yMin->MalmquistBiasTab[[i]]/.Varyx->(\[Sigma]y^2+\[Delta]y^2+(dyOndx*\[Delta]x)^2-2 dyOndx*\[Delta]xy)/.dyOndx->\[Beta]/.{x->xTabBlock[[i]],y->yTab[[i]],\[Delta]x->\[Delta]xTab[[i]],\[Delta]y->xyDataTab[[i,4]],\[Delta]xy->xyDataTab[[i,5]]},{i,nData}]//N//Chop,
#
# Sum[
# (y-Eyx)^2/Varyx+ Log[2.\[Pi]*Varyx/(0.1*VaryxNorm)]+
# (x-\[Mu])^2/(\[Tau]^2+\[Delta]x^2)+Log[2.\[Pi]*(\[Tau]^2+\[Delta]x^2)/(0.1(0.1(xMax-xMin)^2+\[Delta]x\[Mu]^2))]+
# MalmquistOn*(2.Log[0.5Erfc[(yMin-Eyx)/(Sqrt[2.]*Varyx^0.5)]](*-2.Log[UnitStep[y-yMin]+Exp[-200.]]*))/.yMin->MalmquistBiasTab[[i]]/.
# Eyx->\[Alpha]+(\[Beta]*\[Tau]^2+\[Delta]xy)/(\[Tau]^2+\[Delta]x^2) x+(\[Beta]*\[Delta]x^2-\[Delta]xy)/(\[Tau]^2+\[Delta]x^2) \[Mu]/.VaryxSub/.{x->xTabBlock[[i]],\[Delta]x->\[Delta]xTab[[i]],y->yTab[[i]],\[Delta]y->xyDataTab[[i,4]],\[Delta]xy->xyDataTab[[i,5]]},{i,nData}]//N//Chop
# ];
#
# fToFit[{\[Alpha]_,\[Beta]_,\[Sigma]y_,\[Mu]_,\[Tau]_}]:=Evaluate[fChiSquareTmp1];
#
# fChiSquare[p_]:=fToFit[p];
# fPosterior[];
# fMinimumPosterior[Directory[]<>"/kelly_todelete",{\[Alpha]In,\[Beta]In,\[Sigma]yIn,x\[Mu],x\[Sigma]},\[Sigma]FracRandom->10^-2,"n.restarts"->2,"n.iterations"->10^3,"Verbose"->False];
#
# If[\[Sigma]ParTabTmp1=={},\[Sigma]ParTabTmp1={\[Delta]\[Alpha]In,\[Delta]\[Beta]In,0.5*\[Sigma]yIn,0.3*(xMax-xMin),0.3*(xMax-xMin)}
# ];
#
# FileNameTag=Directory[]<>"/kelly_todelete_mcmc";
# xCovMat=DiagonalMatrix[(1/nParFree^0.5*\[Sigma]ParTabTmp1)^2];
#
# fPosteriorMCMCConvergence[Table[If[ParTabBestFitAll[[i]]!=0,ParTabBestFitAll[[i]],10^-3],{i,Length[ParTab]}],xCovMat,nChainsTmp1,nBootv,FileNameTag];
#
# fChainRead=fChainReading[Table[i//ToString,{i,nChainsTmp1}],FileNameTag];
# Run["rm kelly_todelete*"];
#
# DataAllThinnedTab=DataAllTab[[1;;-1;;5]];
# CBITab=CBI[DataAllThinnedTab];
# SBITab=SBI[DataAllThinnedTab];
# MeanTab=Mean[DataAllThinnedTab];StandardDeviationTab=StandardDeviation[DataAllThinnedTab];
# (*{DataMeanTabTmp1=Mean[DataTmp1],StandardDeviation[DataTmp1]}*)
#
# Print[fChainRead];
#
# Print[Table[Histogram[DataAllThinnedTab[[All,i]],Automatic,"ProbabilityDensity",Frame->True,FrameLabel->{ParTagTab[[i]],"pdf"}],{i,IndParChainsFreeTab}]];
#
# Return[{
# {"","\[Alpha]","\[Delta]\[Alpha]","\[Beta]","\[Delta]\[Beta]","\*SubscriptBox[\[Sigma],y]","\*SubscriptBox[\[Delta]\[Sigma],y]","\[Mu]","\[Delta]\[Mu]","\[Tau]","\[Delta]\[Tau]"},
# {"best fit",Table[{ParTabBestFitAll[[i]],"-"},{i,1,nPar}]}//Flatten,
# {"CBI",Table[{CBITab[[i]],SBITab[[i]]},{i,1,nPar}]}//Flatten,
# {"Mean",Table[{MeanTab[[i]],StandardDeviationTab[[i]]},{i,1,nPar}]}//Flatten
# }//Chop//TableForm];
# ];
#
#
# (* ::Text:: *)
# (*Linear regression with the Hogg method*)
#
#
# (*Hogg, y=m*x+b,{m->\[Beta],b->\[Alpha]}*)
# Options[fLinearOrthogonal]={SharpPriors->{},\[Beta]Prior->"StudentT"(*"Uniform"*),
# \[Sigma]ParTab->{}(*\[Sigma]ParTab*),nChains->4,nBoot->10^3};
#
# fLinearOrthogonal[xyDataTab_,OptionsPattern[]]:=Block[
# {MaxParameter=10.^5,ParTab,ParUnitsTab,ParTagTab,PriorsTab,SharpPriorsTabPart1,SharpPriorsTab,
# nData,yFromx,fToFitTmp1,fToFit,ParBestFitSub,
# xPriorv,
# xTab,x\[Mu],x\[Sigma],xMin,xMax,
# yTab,y\[Mu],\[Delta]xTab,\[Delta]x\[Mu],
# \[Alpha],\[Beta],\[Sigma]Orthogonal,
# \[Alpha]In,\[Beta]In,\[Delta]\[Alpha]In,\[Delta]\[Beta]In,\[Sigma]yIn,
# Sin\[Theta],Cos\[Theta],
# vOrthogonal,STab,\[CapitalDelta]Tab,\[CapitalSigma]SquareTab,\[CapitalSigma]SquareTabIn,
# nChainsTmp1,nBootv,xCovMat,CBITab,SBITab,MeanTab,StandardDeviationTab,
# FileNameTag,fChainRead,\[Sigma]ParTabTmp1,
# VaryxNorm},
# \[Sigma]ParTabTmp1=OptionValue[\[Sigma]ParTab];
# nChainsTmp1=OptionValue[nChains];
# nBootv=OptionValue[nBoot];
#
# xTab=xyDataTab[[All,1]];
# x\[Mu]=Mean[xTab];
# x\[Sigma]=StandardDeviation[xTab];
# xMin=Min[xTab];
# xMax=Max[xTab];
#
# yTab=xyDataTab[[All,3]];
# y\[Mu]=Mean[yTab];
#
# \[Delta]xTab=xyDataTab[[All,2]];
# \[Delta]x\[Mu]=Mean[\[Delta]xTab];
#
# (*parameters*)
# ParTab={\[Alpha],\[Beta],\[Sigma]Orthogonal};
# ParUnitsTab={1.,1.,1.};
# ParTagTab={"\[Alpha]","\[Beta]","\!\(\*SubscriptBox[\(\[Sigma]\), \(\[Perpendicular]\)]\)"};
#
# (*priors*)
# PriorsTab={
# {1,"StepPrior",{ParMin->-MaxParameter,ParMax->MaxParameter}},
# {2,"StudentTPrior",{Par\[Nu]->1.}},
# {3,"gtPrior",{ParMin->0,ParMax->1.}//N}
# };
#
# If[OptionValue[\[Beta]Prior]=="Uniform",PriorsTab[[2]]={2,"StepPrior",{ParMin->-MaxParameter,ParMax->MaxParameter}}];
#
# SharpPriorsTab=OptionValue[SharpPriors];
#
# fPriors[];
#
# nData=Length[xyDataTab];
# yFromx=\[Alpha]+\[Beta]*x;
#
# \[Beta]In=Covariance[xTab,yTab]/x\[Sigma]^2;
# \[Alpha]In=y\[Mu]-\[Beta]In*x\[Mu];\[Sigma]yIn=\[Sqrt]((nData-1)/(nData-2.)*(StandardDeviation[yTab ]^2-\[Beta]In*Covariance[xTab,yTab]));
# (*\[Sigma]yIn=Sqrt[1./(nData-2.)*Total[(yTab-(\[Alpha]In+\[Beta]In*xTab))^2]];*)
# \[Delta]\[Alpha]In=\[Sigma]yIn/nData Sqrt[1.+x\[Mu]^2/((nData-1.)/nData x\[Sigma]^2)];
# \[Delta]\[Beta]In=\[Sigma]yIn/(Sqrt[(nData-1.)/nData]*x\[Sigma]);
#
# (*Eq. 29*)
# {Sin\[Theta],Cos\[Theta]}=1./Sqrt[1.+\[Beta]^2] {\[Beta],1.};
# vOrthogonal={-Sin\[Theta],Cos\[Theta]};
# (*Eq .30*)
# \[CapitalDelta]Tab=Table[vOrthogonal.xyDataTab[[i,{1,3}]]-\[Alpha]*Cos\[Theta],{i,nData}]//Simplify;
# (*Eq .26*)
# STab=Table[{{\[Delta]xTab[[i]]^2,xyDataTab[[i,5]]},{xyDataTab[[i,5]],xyDataTab[[i,4]]^2}},{i,nData}]//Simplify;
# (*Eq .31*)
# \[CapitalSigma]SquareTab=Table[vOrthogonal.STab[[i]].vOrthogonal,{i,nData}]//Simplify;
#
# \[CapitalSigma]SquareTabIn=\[CapitalSigma]SquareTab/.{\[Alpha]->\[Alpha]In,\[Beta]->\[Beta]In};
# fToFitTmp1=Total[\[CapitalDelta]Tab^2/(\[CapitalSigma]SquareTab+\[Sigma]Orthogonal^2)]+Total[Log[(\[CapitalSigma]SquareTab+\[Sigma]Orthogonal^2)/(\[CapitalSigma]SquareTabIn/MaxParameter)]];
#
# fToFit[{\[Alpha]_,\[Beta]_,\[Sigma]Orthogonal_}]:=Evaluate[fToFitTmp1];
#
# fChiSquare[p_]:=fToFit[p];
#
# fPosterior[];
#
# fMinimumPosterior[Directory[]<>"/kelly_todelete",{\[Alpha]In,\[Beta]In,\[Sigma]yIn},\[Sigma]FracRandom->10^-2,"n.restarts"->2,"n.iterations"->10^3,"Verbose"->False];
#
# If[\[Sigma]ParTabTmp1=={},\[Sigma]ParTabTmp1={\[Delta]\[Alpha]In,\[Delta]\[Beta]In,0.5*\[Sigma]yIn}];
#
# FileNameTag=Directory[]<>"/kelly_todelete_mcmc";
# xCovMat=DiagonalMatrix[(1/nParFree^0.5*\[Sigma]ParTabTmp1)^2];
#
# fPosteriorMCMCConvergence[Table[If[ParTabBestFitAll[[i]]!=0,ParTabBestFitAll[[i]],10^-3],{i,Length[ParTab]}],xCovMat,nChainsTmp1,nBootv,FileNameTag];
#
# fChainRead=fChainReading[Table[i//ToString,{i,nChainsTmp1}],FileNameTag];
# Run["rm *kelly_todelete*"];
#
# DataAllThinnedTab=DataAllTab[[1;;-1;;5]];
#
# CBITab=CBI[DataAllThinnedTab];
# SBITab=SBI[DataAllThinnedTab];
# MeanTab=Mean[DataAllThinnedTab];
# StandardDeviationTab=StandardDeviation[DataAllThinnedTab];
#
# (*{DataMeanTabTmp1=Mean[DataTmp1],StandardDeviation[DataTmp1]}*)
#
# Print[fChainRead];
# Print[Table[Histogram[DataAllThinnedTab[[All,i]],Automatic,"ProbabilityDensity",Frame->True,FrameLabel->{ParTagTab[[i]],"pdf"}],{i,IndParChainsFreeTab}]];
#
# Return[{
# {"","\[Alpha]","\[Delta]\[Alpha]","\[Beta]","\[Delta]\[Beta]","\!\(\*SubscriptBox[\(\[Sigma]\), \(\[Perpendicular]\)]\)","\!\(\*SubscriptBox[\(\[Delta]\[Sigma]\), \(\[Perpendicular]\)]\)"},
# {"best fit",ParTabBestFitAll[[1]],"-",ParTabBestFitAll[[2]],"-",ParTabBestFitAll[[3]],"-"},
# {"CBI",CBITab[[1]],SBITab[[1]],CBITab[[2]],SBITab[[2]],CBITab[[3]],SBITab[[3]]},
# {"Mean",MeanTab[[1]],StandardDeviationTab[[1]],MeanTab[[2]],StandardDeviationTab[[2]],MeanTab[[3]],StandardDeviationTab[[3]]}
# }//Chop//TableForm];
#
# ];
#
#
# (* ::Text:: *)
# (*Linear regression with a binomial method*)
#
#
# Options[fLinearBinomial]={SharpPriors->{},\[Beta]Prior->"StudentT"(*"Uniform"*),
# \[Sigma]ParTab->{}(*\[Sigma]ParTab*),nChains->4,nBoot->10^3};
#
# fLinearBinomial[xyDataTab_,OptionsPattern[]]:=Block[{
# MaxParameter=10.^5,ParTab,ParUnitsTab,ParTagTab,PriorsTab,SharpPriorsTabPart1,SharpPriorsTab,
# (*nData,fToFit,*)ParBestFitSub,
# xTab,x\[Mu],x\[Sigma],yTab,y\[Mu],
# \[Alpha]In,\[Beta]In,\[Delta]\[Alpha]In,\[Delta]\[Beta]In,\[Sigma]yIn,nChainsTmp1,nBootv,xCovMat,
# CBITab,SBITab,MeanTab,StandardDeviationTab,
# FileNameTag,fChainRead,\[Sigma]ParTabTmp1,VaryxSub,VaryxNorm},
#
# \[Sigma]ParTabTmp1=OptionValue[\[Sigma]ParTab];
# nChainsTmp1=OptionValue[nChains];
# nBootv=OptionValue[nBoot];
#
# nData=Length[xyDataTab];
# xTab=xyDataTab[[All,1]];
# x\[Mu]=Mean[xTab];
# x\[Sigma]=StandardDeviation[xTab];
#
# yTab=xyDataTab[[All,3]];
# y\[Mu]=Mean[yTab];
#
# (*parameters*)
# ParTab={\[Alpha],\[Beta]};
# ParUnitsTab={1.,1.};
# ParTagTab={"\[Alpha]","\[Beta]"};
#
# (*priors*)
# PriorsTab={
# {1,"StepPrior",{ParMin->-MaxParameter,ParMax->MaxParameter}},
# {2,"StudentTPrior",{Par\[Nu]->1.}}
# };
#
# If[OptionValue[\[Beta]Prior]=="Uniform",PriorsTab[[2]]={2,"StepPrior",{ParMin->-MaxParameter,ParMax->MaxParameter}}];
#
# SharpPriorsTab=OptionValue[SharpPriors];
#
# fPriors[];
#
# fLikeBinomial[{\[Alpha]1_,\[Beta]1_}]:=Block[{yThTab,nUp},
# yThTab=\[Alpha]1+\[Beta]1*xyToFitTabTmp1[[All,1]];
# nUp=Length[Select[yTab-yThTab,#>0.&]];
# PDF[BinomialDistribution[nData,0.5],nUp]
# ];
#
#
# \[Beta]In=Covariance[xTab,yTab]/x\[Sigma]^2;
# \[Alpha]In=y\[Mu]-\[Beta]In*x\[Mu];
# \[Sigma]yIn=\[Sqrt]((nData-1)/(nData-2.)*(StandardDeviation[yTab ]^2-\[Beta]In*Covariance[xTab,yTab]));\[Delta]\[Alpha]In=\[Sigma]yIn/nData Sqrt[1.+x\[Mu]^2/((nData-1.)/nData x\[Sigma]^2)];
# \[Delta]\[Beta]In=\[Sigma]yIn/(Sqrt[(nData-1.)/nData]*x\[Sigma]);
#
# fChiSquare[p_]:=-2.*Log[fLikeBinomial[p]];
# fPosterior[];
# fMinimumPosterior[Directory[]<>"/kelly_todelete",{\[Alpha]In,\[Beta]In},\[Sigma]FracRandom->10^-2,"n.restarts"->2,"n.iterations"->10^3,"Verbose"->False];
#
# If[\[Sigma]ParTabTmp1=={},\[Sigma]ParTabTmp1={\[Delta]\[Alpha]In,\[Delta]\[Beta]In}
# ];
#
# FileNameTag=Directory[]<>"/kelly_todelete_mcmc";
# xCovMat=DiagonalMatrix[(1/nParFree^0.5*\[Sigma]ParTabTmp1)^2];
#
# fPosteriorMCMCConvergence[Table[If[ParTabBestFitAll[[i]]!=0,ParTabBestFitAll[[i]],1./MaxParameter],{i,Length[ParTab]}],xCovMat,nChainsTmp1,nBootv,FileNameTag];
#
# fChainRead=fChainReading[Table[i//ToString,{i,nChainsTmp1}],FileNameTag];
# Run["rm kelly_todelete*"];
#
# DataAllThinnedTab=DataAllTab[[1;;-1;;5]];
# CBITab=CBI[DataAllThinnedTab];
# SBITab=SBI[DataAllThinnedTab];
# MeanTab=Mean[DataAllThinnedTab];StandardDeviationTab=StandardDeviation[DataAllThinnedTab];
# (*{DataMeanTabTmp1=Mean[DataTmp1],StandardDeviation[DataTmp1]}*)
#
# Print[fChainRead];
#
# Print[Table[Histogram[DataAllThinnedTab[[All,i]],Automatic,"ProbabilityDensity",Frame->True,FrameLabel->{ParTagTab[[i]],"pdf"}],{i,IndParChainsFreeTab}]];
#
# Return[{
# {"","\[Alpha]","\[Delta]\[Alpha]","\[Beta]","\[Delta]\[Beta]"},
# {"best fit",Table[{ParTabBestFitAll[[i]],"-"},{i,1,nPar}]}//Flatten,
# {"CBI",Table[{CBITab[[i]],SBITab[[i]]},{i,1,nPar}]}//Flatten,
# {"Mean",Table[{MeanTab[[i]],StandardDeviationTab[[i]]},{i,1,nPar}]}//Flatten
# }//Chop//TableForm];
# ];
#
#
# Begin["`Private`"];
#
#
# fMinimumDownHill[fToMin_,nParameters_,xTabIn_,FilePattern_(*Pattern name for the output files*),
# OptionsPattern[]]:=Module[
# {FileSimplPattern,FileDownPattern,OutDownStrem,\[Alpha]Reflv,\[Gamma]Expav,\[Beta]Contv,\[Delta]Shriv,nIntv=1,\[Delta]xv,\[Delta]fv,fTiny=10^-10,
# xRefl,xCentr,xExpa,xContr,xShri,fRefl,fExpa,fContr,fMean,xTab,\[Delta]xTab,xTabToExpo,
# nDownv,nRestarts,xTolv,fTolv,Suffixv},
#
# {\[Alpha]Reflv,\[Gamma]Expav,\[Beta]Contv,\[Delta]Shriv}={1,2,1/2,1/2}//N;
#
# nDownv=OptionValue["n.iterations"](* # max iterations*);
# nRestarts=OptionValue["n.restarts"](* # restarv*);
# xTolv=OptionValue["xTolerance"] (* fractional tolerance for vector distance *);
# \[Delta]xv=xTolv+1.;
# fTolv=OptionValue["fTolerance"](* fractional tolerance for function values *);
# \[Delta]fv=fTolv+1.;
# Suffixv=OptionValue[Suffix];
#
# FileSimplPattern=FilePattern<>"_simplex";
# (*
# FileDownPattern=FilePattern<>"_down";
# OutDownStrem= OpenWrite[FileDownPattern<>"_"<>Suffixv<>".dat"];
# *)
#
# xTab=Sort[xTabIn,#1[[2]]<#2[[2]]&];
#
# While[nIntv<=nDownv && \[Delta]xv>=xTolv && \[Delta]fv>=fTolv,
# (*centroid*)
# xCentr=Mean[xTab[[1;;nParameters,1]]];
# (*reflection*)
# xRefl=xCentr+\[Alpha]Reflv*(xCentr-xTab[[-1,1]]);fRefl=fToMin[xRefl];
#
# If[fRefl<=xTab[[1,2]],
# (*Put["expansion computed",OutDownStrem];*)
# xExpa=xCentr+\[Gamma]Expav*(xRefl-xCentr);fExpa=fToMin[xExpa];
# If[fExpa<fRefl,
# (*Put["expansion accepted",OutDownStrem];*)
# xTab=Join[{{xExpa,fExpa}},xTab[[1;;nParameters]]],
# (*Put["expansion refused",OutDownStrem];*)
# xTab=Join[{{xRefl,fRefl}},xTab[[1;;nParameters]]]
# ],
# If[fRefl>=xTab[[nParameters,2]],
# If[fRefl<xTab[[nParameters+1,2]],
# (*Put["contraction outside computed",OutDownStrem];*)
# xTab=Join[xTab[[1;;nParameters]],{{xRefl,fRefl}}]
# (*,Put["contraction inside computed",OutDownStrem]*)
# ];
# xContr=xCentr+\[Beta]Contv*(xTab[[-1,1]]-xCentr);fContr=fToMin[xContr];
# If[fContr>=xTab[[-1,2]],
# (*Put["shrinking",OutDownStrem];*)
# xTab=Sort[Join[{xTab[[1]]},Table[{xShri=xTab[[1,1]]+\[Delta]Shriv(xTab[[i,1]]-xTab[[1,1]]),fToMin[xShri]}//N,{i,2,nParameters+1}]],#1[[2]]<#2[[2]]&],
# (*Put["contraction accepted",OutDownStrem];*)
# xTab= Sort[Join[{{xContr,fContr}},xTab[[1;;nParameters]]],#1[[2]]<#2[[2]]&]
# ],
# (*Write[OutDownStrem,"reflection accepted"];*)
# xTab=Sort[Join[{{xRefl,fRefl}},xTab[[1;;nParameters]]],#1[[2]]<#2[[2]]&]
# ]
# ];
# (*
# Which[
# xTab[[1,2]]<fRefl\[LessEqual] xTab[[nParameters,2]],
# (*Write[OutDownStrem,"reflection accepted"];*)
# xTab=Sort[Join[{{xRefl,fRefl}},xTab[[1;;nParameters]]],#1[[2]]<#2[[2]]&],
# fRefl<=xTab[[1,2]],
# (*Put["expansion computed",OutDownStrem];*)
# xExpa=xCentr+\[Gamma]Expav*(xRefl-xCentr);
# fExpa=fToMin[xExpa];
# If[fExpa<fRefl,(*Put["expansion accepted",OutDownStrem];*)
# xTab=Join[{{xExpa,fExpa}},xTab[[1;;nParameters]]],
# (*Put["expansion refused",OutDownStrem];*)
# xTab=Join[{{xRefl,fRefl}},xTab[[1;;nParameters]]]
# ],
# fRefl>xTab[[nParameters,2]],
# If[fRefl<xTab[[-1,2]],xTab=Join[xTab[[1;;nParameters]],{{xRefl,fRefl}}]];xContr=xCentr+\[Beta]Contv*(xTab[[-1,1]]-xCentr);fContr=fToMin[xContr];
# If[fContr\[LessEqual]xTab[[-1,2]],
# (*Put["contraction accepted",OutDownStrem];*)
# xTab=Sort[Join[{{xContr,fContr}},xTab[[1;;nParameters]]],#1[[2]]<#2[[2]]&],
# (*Put["shrinking",OutDownStrem];*)
# xTab=Sort[Join[{xTab[[1]]},Table[{xShri=xTab[[1,1]]+\[Delta]Shriv(xTab[[i,1]]-xTab[[1,1]]),fToMin[xShri]}//N,{i,2,nParameters+1}]],#1[[2]]<#2[[2]]&]
# ]
# ]
# *)
#
# \[Delta]fv=Sqrt[Variance[xTab[[All,2]]]]/Abs[xTab[[1,2]]];
# \[Delta]xv=Sum[Total[(xTab[[i,1]]-xTab[[1,1]])^2]^0.5,{i,2,nParameters+1}]/Total[xTab[[1,1]]^2]^.5;
#
# (* exporting simplex *)
# If[OptionValue[Verbose]=="True",Export[FileSimplPattern<>"_"<>Suffixv<>".dat",Table[xTab[[i]]//Flatten,{i,nParameters+1}]]];
# (*Put[{nIntv,\[Delta]fv},xTab[[1]],OutDownStrem];*)
# nIntv++];
# Export[FileSimplPattern<>"_"<>Suffixv<>".dat",Table[xTab[[i]]//Flatten,{i,nParameters+1}]];
# (*Close[OutDownStrem];*)
# Return[{xTab,nIntv}]
# ];
#
#
# fMCMC[xParInTab_,\[Sigma]xPar_,Likelihood_,nBoot_,FileString_]:=Module[
# {nBootMin=Min[nBoot-1,2*10^3],nSamples,
# nPar=Length[xParInTab[[1]]],nChains=Length[xParInTab],
# xPar1Tab=xParInTab,xPar2Tab,p1Tab,p1v,p2v,xPar2,acc,
# ParTab,ParProbTab,ParTabAll,ParTabLastN,ParMeanChain,ParMeanAll,Bn,W,
# nFirst,nGelRub,RGelRubTab,RGelRubMax=1.2,
# NumKernTab=CharacterRange["1","9"],
# FileOutTab,OutStremTab},
#
# nSamples=If[Run["ls",FileString<>"_1.dat"]==0,2,1];
# FileOutTab=Table[FileString<>"_"<>NumKernTab[[i]]<>".dat",{i,nChains}];
# OutStremTab=Table[{},{i,nChains}];
# Do[
# OutStremTab[[j]]=OpenAppend[FileOutTab[[j]]],
# {j,nChains}
# ];
#
# If[
# nSamples==1,
# p1Tab=Table[Likelihood[xPar1Tab[[i]]],{i,nChains}];
# Do[
# Do[
# WriteString[OutStremTab[[j]],InputForm[xPar1Tab[[j,i]]]," "],
# {i,nPar}
# ];
# Write[OutStremTab[[j]],p1Tab[[j]]],
# {j,nChains}
# ]
# ];
#
# ParProbTab=Table[ Import[FileOutTab[[i]],"Table"],{i,nChains}];
# ParTab=Table[ParProbTab[[i]][[All,1;;nPar]],{i,nChains}];
# nSamples=Max[1,Length[ParTab[[1]] ]   ];
#
# xPar1Tab=If[nSamples>1,Table[ParTab[[i,nSamples  ]],{i,nChains}],xParInTab];
# If[nSamples>1,p1Tab=Table[ParProbTab[[i,nSamples,nPar+1  ]],{i,nChains}]];
#
# RGelRubTab=Table[RGelRubMax+1,{nPar}];
# While[nSamples<nBoot &&(Select[Table[RGelRubTab[[i]]<RGelRubMax,{i,nPar}],#==False&]!= {}),
# Do[
# p1v=p1Tab[[j]];
# xPar2=Table[RandomReal[NormalDistribution[xPar1Tab[[j,k]],\[Sigma]xPar[[k]] ]],{k,nPar}];
# p2v=Likelihood[xPar2];
# acc= RandomReal[];
# (*Print[{i,ChiSquareTmp1,ChiSquareTmp2,p2v/p1v,acc,acc<=Min[1,(p2v/p1v)]}];*)
# If[
# acc<=Min[1,(p2v/p1v)],
# xPar1Tab[[j]]=xPar2;p1Tab[[j]]=p2v
# ];
# AppendTo[ParTab[[j]],xPar1Tab[[j]]  ];
#
# Do[
# WriteString[OutStremTab[[j]],InputForm[xPar1Tab[[j,i]]]," "],
# {i,nPar}];
# Write[OutStremTab[[j]],p1Tab[[j]]],
# {j,nChains}];
#
# If[nSamples>=nBootMin,
# (*nFirst=IntegerPart[nSamples/2]; (*Verde et al.. (2003)*)*)
# nFirst=nBootMin/2;(*croll et al.. (2003)*)
# nGelRub=nSamples-nFirst;
# ParTabLastN=Table[ Drop[ParTab[[j]],nFirst],{j,nChains}];
# ParTabAll=Partition[ParTabLastN//Flatten,nPar];
#
# ParMeanChain=Table[Mean[ParTabLastN[[j]] ],{j,nChains}];
# ParMeanAll=Mean[ParTabAll ];
# Bn=1/(nChains-1) Sum[(ParMeanChain[[j]]-ParMeanAll)^2,{j,nChains}];(*variance between chains*)
# W=1/(nChains(nGelRub-1)) Sum[(ParTabLastN[[j,i]]-ParMeanChain[[j]])^2,{i,nGelRub},{j,nChains}];  (*variance within a chain*)
# RGelRubTab=1/W (((nGelRub-1)/nGelRub)W+Bn(1+1/nChains))
# ];
# nSamples++
# ];
# Do[
# Close[OutStremTab[[j]]],
# {j,nChains}];
#
# {ParTab,{Bn,W,RGelRubTab}}
# ];
#
#
# fMCMCNoConv[xParInTab_,\[Sigma]xPar_,Likelihood_,nBoot_,FileString_]:=Module[
# {nSamples,
# nPar=Length[xParInTab[[1]]],nChains=Length[xParInTab],
# xPar1Tab=xParInTab,xPar2Tab,p1Tab,p1v,p2v,xPar2,acc,
# ParTab,
# NumKernTab=CharacterRange["1","9"],
# FileOutTab,OutStremTab},
#
# nSamples=If[Run["ls",FileString<>"_1.dat"]==0,2,1];
# FileOutTab=Table[FileString<>"_"<>NumKernTab[[i]]<>".dat",{i,nChains}];
# OutStremTab=Table[{},{i,nChains}];
# Do[
# OutStremTab[[j]]=OpenAppend[FileOutTab[[j]]],
# {j,nChains}
# ];
#
# If[
# nSamples==1,
# p1Tab=Table[Likelihood[xPar1Tab[[i]]],{i,nChains}];
# Do[
# Do[
# WriteString[OutStremTab[[j]],InputForm[xPar1Tab[[j,i]]]," "],
# {i,nPar}
# ];
# Write[OutStremTab[[j]],OutputForm[ExportString[p1Tab[[j]],"Table"]]],
# {j,nChains}
# ]
# ];
#
# ParTab=Table[ Import[FileOutTab[[i]],"Table"],{i,nChains}];
# nSamples=Max[1,Length[ParTab[[1]] ]   ];
#
# xPar1Tab=If[nSamples>1,Table[ParTab[[i,nSamples  ]][[1;;nPar]],{i,nChains}],xParInTab];
# If[nSamples>1,p1Tab=Table[ParTab[[i,nSamples,nPar+1  ]],{i,nChains}]];
#
# Do[
# Do[
# p1v=p1Tab[[j]];
# xPar2=Table[RandomReal[NormalDistribution[xPar1Tab[[j,k]],\[Sigma]xPar[[k]] ]],{k,nPar}];
# p2v=Likelihood[xPar2];
# acc= RandomReal[];
# (*Print[{i,ChiSquareTmp1,ChiSquareTmp2,p2v/p1v,acc,acc<=Min[1,(p2v/p1v)]}];*)
# If[
# acc<=Min[1,(p2v/p1v)],
# xPar1Tab[[j]]=xPar2;p1Tab[[j]]=p2v
# ];
#
# Do[
# WriteString[OutStremTab[[j]],InputForm[xPar1Tab[[j,i]]]," "],
# {i,nPar}];
# Write[OutStremTab[[j]],OutputForm[ExportString[p1Tab[[j]],"Table"]]],
# {j,nChains}],
# {IndSample,nSamples+1,nBoot}
# ];
#
# Do[
# Close[OutStremTab[[j]]],
# {j,nChains}];
# ];
#
#
# fMCMCNoConvExport[xParInTab_,\[Sigma]xPar_,Likelihood_,nBoot_,FileString_]:=Module[
# {PreChainCheck,nSamplesTab,
# nPar=Length[xParInTab[[1]]],nChains=Length[xParInTab],
# xPar1Tab=xParInTab,xPar2Tab,xParTmp1,p1Tab,p1v,p2v,pTmp1,xPar2,acc,
# ParTab,
# NumKernTab=CharacterRange["1","9"],
# OutPutPreTab,OutPutPostTab,
# FileOutTab},
#
# PreChainCheck=If[Run["ls",FileString<>"_1.dat"]==0,2,1];
# FileOutTab=Table[FileString<>"_"<>NumKernTab[[i]]<>".dat",{i,nChains}];
#
# If[
# PreChainCheck==1,
# OutPutPreTab=Table[{{xParInTab[[j]],Likelihood[xParInTab[[j]]]}//Flatten},{j,nChains}],
# OutPutPreTab=Table[Import[FileOutTab[[j]],"Table" ],{j,nChains}]
# (*OutPutTab=Table[Partition[ReadList[FileOutTab[[j]] ,Real],nPar+1],{j,nChains}]*)
# ];
#
# nSamplesTab=Table[Length[OutPutPreTab[[j]]],{j,nChains}];
#
# xPar1Tab=Table[OutPutPreTab[[i,nSamplesTab[[i]]  ]][[1;;nPar]],{i,nChains}];
# p1Tab=Table[OutPutPreTab[[j,nSamplesTab[[j]],nPar+1  ]],{j,nChains}];
#
# Do[
# pTmp1=p1Tab[[j]];
# xParTmp1=xPar1Tab[[j]];
# OutPutPostTab=Reap[
# Do[
# p1v=pTmp1;
# xPar2=Table[RandomReal[NormalDistribution[xParTmp1[[k]],\[Sigma]xPar[[k]] ]],{k,nPar}];
# p2v=Likelihood[xPar2];
# acc= RandomReal[];
# If[
# acc<=Min[1.,(p2v/p1v)],
# xParTmp1=xPar2;pTmp1=p2v
# ];
# Sow[{xParTmp1,pTmp1}//Flatten],
# {IndSample,nSamplesTab[[j]]+1,nBoot}
# ];
# ][[2,1]];
# Export[FileOutTab[[j]],Join[OutPutPreTab[[j]],OutPutPostTab]];
# Clear[OutPutPostTab],
# {j,nChains}];
#
# ];
#
#
# Options[fGelmanRubin]={"n.adapt"->0};
# fGelmanRubin[ParTab_,OptionsPattern[]]:=Block[
# {nFirst,nChains,nSamples,nPar,ParTabLastN,ParTabAll,ParMeanChain,ParMeanAll,Bn,W,nGelRub,RGelRubTab},
#
# nChains=Length[ParTab];
# nSamples=Length[ParTab[[1]]];
# nFirst=OptionValue["n.adapt"];
#
# nGelRub=nSamples-nFirst;
#
# ParTabLastN=Table[ Drop[ParTab[[j]],nFirst],{j,nChains}];
# ParTabAll=Partition[ParTabLastN//Flatten,Length[ParTab[[1,1]]]];
# (*ParTabAll=Join[Table[ParTabLastN[[j]],{j,nChains}]];*)
#
# ParMeanChain=Map[Mean[# ]&,ParTabLastN];
# ParMeanAll=Mean[ParMeanChain];
# Bn=1./(nChains-1.) Sum[(ParMeanChain[[j]]-ParMeanAll)^2,{j,nChains}];(*variance between chains*)
# W=1./(nChains*(nGelRub-1.)) Sum[(ParTabLastN[[j,i]]-ParMeanChain[[j]])^2,{i,nGelRub},{j,nChains}];  (*variance within a chain*)
# RGelRubTab=1./W (((nGelRub-1.)/nGelRub)W+Bn(1.+1./nChains));
# {Bn,W,RGelRubTab}
# ];
#
#
# fRGelmann[FileString_,nChains_,nFirst_]:=Module[
# {nSamples,nPar,ParTab,ParTabLastN,ParTabAll,ParMeanChain,ParMeanAll,Bn,W,nGelRub,RGelRubTab},
#
# ParTab=Table[Import[FileString<>"_"<>CharacterRange["1","9"][[i]]<>".dat","Table"],{i,nChains}];
#
# nSamples=Length[ParTab[[1]]];
# nPar=Length[ParTab[[1,1]]]-1;
# (*
# nFirst=0;
# nFirst=IntegerPart[nSamples/2];
# *)
#
# nGelRub=nSamples-nFirst;
#
# ParTabLastN=Table[ Drop[ParTab[[j]][[All,1;;nPar]],nFirst],{j,nChains}];
# ParTabAll=Partition[ParTabLastN//Flatten,nPar];
#
# ParMeanChain=Table[Mean[ParTabLastN[[j]] ],{j,nChains}];
# ParMeanAll=Mean[ParTabAll ];
# Bn=1/(nChains-1) Sum[(ParMeanChain[[j]]-ParMeanAll)^2,{j,nChains}];(*variance between chains*)
# W=1/(nChains(nGelRub-1)) Sum[(ParTabLastN[[j,i]]-ParMeanChain[[j]])^2,{i,nGelRub},{j,nChains}];  (*variance within a chain*)
#
# RGelRubTab=1/W (((nGelRub-1)/nGelRub)W+Bn(1+1/nChains));
# {nSamples,Bn,W,RGelRubTab}
# ];
#
#
# Options[fGelmanRubin]={"n.adapt"->0};
# fGelmanRubin[ParTab_,OptionsPattern[]]:=Block[
# {nFirst,nChains,nSamples,nPar,ParTabLastN,ParTabAll,ParMeanChain,ParMeanAll,Bn,W,nGelRub,RGelRubTab},
#
# nChains=Length[ParTab];
# nSamples=Length[ParTab[[1]]];
# nFirst=OptionValue["n.adapt"];
#
# nGelRub=nSamples-nFirst;
#
# ParTabLastN=Table[ Drop[ParTab[[j]],nFirst],{j,nChains}];
# ParTabAll=Partition[ParTabLastN//Flatten,Length[ParTab[[1,1]]]];
# (*ParTabAll=Join[Table[ParTabLastN[[j]],{j,nChains}]];*)
#
# ParMeanChain=Map[Mean[# ]&,ParTabLastN];
# ParMeanAll=Mean[ParMeanChain];
# Bn=1./(nChains-1.) Sum[(ParMeanChain[[j]]-ParMeanAll)^2,{j,nChains}];(*variance between chains*)
# W=1./(nChains*(nGelRub-1.)) Sum[(ParTabLastN[[j,i]]-ParMeanChain[[j]])^2,{i,nGelRub},{j,nChains}];  (*variance within a chain*)
# RGelRubTab=1./W (((nGelRub-1.)/nGelRub)W+Bn(1.+1./nChains));
# {Bn,W,RGelRubTab}
# ];
#
#
# fRGelmannParTab[ParTab_,nChains_,nFirst_]:=Module[
# {nSamples,nPar,ParTabLastN,ParTabAll,ParMeanChain,ParMeanAll,Bn,W,nGelRub,RGelRubTab},
#
# nSamples=Length[ParTab[[1]]];
# nPar=Length[ParTab[[1,1]]]-1;
# (*
# nFirst=0;
# nFirst=IntegerPart[nSamples/2];
# *)
#
# nGelRub=nSamples-nFirst;
#
# ParTabLastN=Table[ Drop[ParTab[[j]][[All,1;;nPar]],nFirst],{j,nChains}];
# ParTabAll=Partition[ParTabLastN//Flatten,nPar];
#
# ParMeanChain=Table[Mean[ParTabLastN[[j]] ],{j,nChains}];
# ParMeanAll=Mean[ParTabAll ];
# Bn=1/(nChains-1) Sum[(ParMeanChain[[j]]-ParMeanAll)^2,{j,nChains}];(*variance between chains*)
# W=1/(nChains(nGelRub-1)) Sum[(ParTabLastN[[j,i]]-ParMeanChain[[j]])^2,{i,nGelRub},{j,nChains}];  (*variance within a chain*)
# RGelRubTab=1/W (((nGelRub-1)/nGelRub)W+Bn(1+1/nChains));
# Clear[ParTabLastN,ParTabAll,ParTabLastN];
# {nSamples,Bn,W,RGelRubTab}
# ];
#
#
# fkaMultiBound[\[Eta]_,TabY_,\[Eta]Min_,\[Eta]Max_]:=Module[{d,NSample,\[Sigma]tSilverman,KGausNorm,KGausBound},
# d=If[Length[TabY[[1]]]>0,Length[TabY[[1]]],1];
# NSample=Length[TabY];
# \[Sigma]tSilverman=0.96  NSample^(-1/(d+4)) StandardDeviation[TabY];
# (*
# \[Sigma]tSilverman=0.96  NSample^(-1/(d+4)) StandardDeviation[TabY];
# \[Sigma]tSilverman=If[d>1,0.96  NSample^(-1/(d+4)) (1/d StandardDeviation[TabY].StandardDeviation[TabY])^(1/2),0.96  NSample^(-1/5) StandardDeviation[TabY]];
# *)
# KGausNorm[x_]:=If[d>1,1/(2\[Pi])^(d/2) Exp[-(x.x/2)],1/Sqrt[2\[Pi]] Exp[-(x^2/2)]];
# KGausBound[\[Eta]Mean_,\[Sigma]_]:=If[d>1,1/Product[\[Sigma][[i]],{i,d}],1/\[Sigma]]*(*If[d>1,1/Product[\[Sigma][[i]],{i,d}],1/\[Sigma]],1/\[Sigma]*)(KGausNorm[(\[Eta]-\[Eta]Mean)/\[Sigma]]+KGausNorm[(\[Eta]+\[Eta]Mean-2\[Eta]Min)/\[Sigma]]+KGausNorm[(2\[Eta]Max-\[Eta]-\[Eta]Mean)/\[Sigma]]);
# 1/ NSample Sum[KGausBound[TabY[[i]],\[Sigma]tSilverman  ],{i,NSample}]
# ];
#
#
# (*fkaMultiBoundBinned[\[Eta]_,DataTab_,\[Eta]Min_,\[Eta]Max_,nSteps_]:=Module[{d,NSample,\[Sigma]tSilverman,MaxDataTab,MinDataTab,MeanDataTab,SigmaDataTab,DataInTab,(*StepTab,*)n\[Sigma]v=1/2,IndPermTab,KGausNorm,KGausBound,nPar,nBins(*,CentersTab,BinnedDataTab*)},
# nPar=If[Length[DataTab[[1]]]>0,Length[DataTab[[1]]],1];
# NSample=Length[DataTab];
# (*\[Sigma]tSilverman=0.96  NSample^(-1/(nPar+4)) StandardDeviation[DataTab];*)
# (*nSteps=Round[nBinsTmp1^(1/nPar)];*)
# nBins=Product[nSteps[[i]],{i,nPar}];
# (*
# MeanDataTab=Mean[DataTab];
# SigmaDataTab=StandardDeviation[DataTab];
# *)
# MaxDataTab=Table[Max[DataTab[[All,i]]],{i,nPar}];
# MinDataTab=Table[Min[DataTab[[All,i]]],{i,nPar}];
# StepTab=(MaxDataTab-MinDataTab)/nSteps;
# \[CapitalDelta]Tab=Table[Table[MinDataTab[[i]]+j*StepTab[[i]],{j,-1,nSteps[[i]]+1}],{i,nPar}];
# BinCounts[DataTab,\[CapitalDelta]Tab]
#
# (*
# IndPermTab=Permutations[ConstantArray[Range[-2,nSteps+1],nPar]//Flatten//Sort,{nPar}];
# nBins=Length[IndPermTab];
# DataInTab=MinDataTab*Table[1+RandomReal[{-10^-3,10^-3}],{i,nPar}];
# CentersTab=Table[DataInTab+StepTab(ConstantArray[1/2,nPar]+IndPermTab[[i]]),{i,nBins}];
# BinnedDataTab=Table[{CentersTab[[j]],Length[Select[DataTab,Table[Abs[(#-CentersTab[[j]])[[i]]]<=n\[Sigma]v*StepTab[[i]],{i,nPar}]==ConstantArray[True,nPar]&]]},{j,nBins}];
# *)
# (*
# KGausNorm[x_]:=If[nPar>1,1/(2\[Pi])^(nPar/2) Exp[-(x.x/2)],1/Sqrt[2\[Pi]] Exp[-(x^2/2)]];
# KGausBound[\[Eta]Mean_,\[Sigma]_]:=If[nPar>1,1/Product[\[Sigma][[i]],{i,nPar}],1/\[Sigma]]*(*If[d>1,1/Product[\[Sigma][[i]],{i,d}],1/\[Sigma]],1/\[Sigma]*)(KGausNorm[(\[Eta]-\[Eta]Mean)/\[Sigma]]+KGausNorm[(\[Eta]+\[Eta]Mean-2\[Eta]Min)/\[Sigma]]+KGausNorm[(2\[Eta]Max-\[Eta]-\[Eta]Mean)/\[Sigma]]);
# {1/ NSample Sum[BinnedDataTab[[i,2]]*KGausBound[BinnedDataTab[[i,1]],StepTab  ],{i,nBins}],Interpolation[Partition[BinnedDataTab//Flatten,nPar+1]]}
# *)
# ];
# *)
#
# (*
# fkaMultiBoundBinned[\[Eta]_,DataTab_,\[Eta]Min_,\[Eta]Max_,nBinsTmp1_]:=Module[{d,NSample,\[Sigma]tSilverman,MaxDataTab,MinDataTab,MeanDataTab,SigmaDataTab,StepTab,n\[Sigma]v=1/2,IndPermTab,KGausNorm,KGausBound,nSteps,nPar,nBins,CentersTab,BinnedDataTab},
# nPar=If[Length[DataTab[[1]]]>0,Length[DataTab[[1]]],1];
# NSample=Length[DataTab];
# (*\[Sigma]tSilverman=0.96  NSample^(-1/(nPar+4)) StandardDeviation[DataTab];*)
#
# nSteps=Round[nBinsTmp1^(1/nPar)];
# (*
# MeanDataTab=Mean[DataTab];
# SigmaDataTab=StandardDeviation[DataTab];
# *)
# MaxDataTab=Table[Max[DataTab[[All,i]]],{i,nPar}];
# MinDataTab=Table[Min[DataTab[[All,i]]],{i,nPar}];
# StepTab=(MaxDataTab-MinDataTab)/nSteps;
# IndPermTab=Permutations[ConstantArray[Range[0,nSteps-1],nPar]//Flatten//Sort,{nPar}];
# nBins=Length[IndPermTab];
# CentersTab=Table[MinDataTab+StepTab(ConstantArray[1/2,nPar]+IndPermTab[[i]]),{i,nBins}];
# BinnedDataTab=Table[{CentersTab[[j]],Length[Select[DataTab,Table[Abs[(#-CentersTab[[j]])[[i]]]<=n\[Sigma]v*StepTab[[i]],{i,nPar}]==ConstantArray[True,nPar]&]]},{j,nBins}];
# KGausNorm[x_]:=If[nPar>1,1/(2\[Pi])^(nPar/2) Exp[-(x.x/2)],1/Sqrt[2\[Pi]] Exp[-(x^2/2)]];
# KGausBound[\[Eta]Mean_,\[Sigma]_]:=If[nPar>1,1/Product[\[Sigma][[i]],{i,nPar}],1/\[Sigma]]*(*If[d>1,1/Product[\[Sigma][[i]],{i,d}],1/\[Sigma]],1/\[Sigma]*)(KGausNorm[(\[Eta]-\[Eta]Mean)/\[Sigma]]+KGausNorm[(\[Eta]+\[Eta]Mean-2\[Eta]Min)/\[Sigma]]+KGausNorm[(2\[Eta]Max-\[Eta]-\[Eta]Mean)/\[Sigma]]);
# 1/ NSample Sum[BinnedDataTab[[i,2]]*KGausBound[BinnedDataTab[[i,1]],StepTab/3  ],{i,nBins}]
# ];
# *)
#
#
# fkaMultiBoundBinned[\[Eta]_,DataTab_,\[Eta]Min_,\[Eta]Max_,nSteps_]:=Module[
# {d,NSample,\[Sigma]tSilverman,MaxDataTab,MinDataTab,MeanDataTab,SigmaDataTab,StepTab,n\[Sigma]v=1/2,IndPermTab,KGausNorm,KGausBound,nPar,nBins,DataInTab,CentersTab,BinnedDataTab},
# nPar=If[Length[DataTab[[1]]]>0,Length[DataTab[[1]]],1];
# NSample=Length[DataTab];
# (*\[Sigma]tSilverman=0.96  NSample^(-1/(nPar+4)) StandardDeviation[DataTab];*)
#
# (*nSteps=Round[nBinsTmp1^(1/nPar)];*)
# (*
# MeanDataTab=Mean[DataTab];
# SigmaDataTab=StandardDeviation[DataTab];
# *)
# MaxDataTab=Table[Max[DataTab[[All,i]]],{i,nPar}];
# MinDataTab=Table[Min[DataTab[[All,i]]],{i,nPar}];
# StepTab=(MaxDataTab-MinDataTab)/nSteps;
# (*IndPermTab=Permutations[ConstantArray[Range[0,nSteps-1],nPar]//Flatten//Sort,{nPar}];*)
# IndPermTab=Tuples[Table[Range[-1,nSteps[[i]]+1],{i,nPar}]];
# nBins=Length[IndPermTab];
# DataInTab=MinDataTab*Table[1+RandomReal[{-10^-3,10^-3}],{i,nPar}];
# CentersTab=Table[DataInTab+StepTab(ConstantArray[1/2,nPar]+IndPermTab[[i]]),{i,nBins}];
# BinnedDataTab=Table[{CentersTab[[j]],Length[Select[DataTab,Table[Abs[(#-CentersTab[[j]])[[i]]]<=n\[Sigma]v*StepTab[[i]],{i,nPar}]==ConstantArray[True,nPar]&]]},{j,nBins}];
# KGausNorm[x_]:=If[nPar>1,1/(2\[Pi])^(nPar/2) Exp[-(x.x/2)],1/Sqrt[2\[Pi]] Exp[-(x^2/2)]];
# KGausBound[\[Eta]Mean_,\[Sigma]_]:=If[nPar>1,1/Product[\[Sigma][[i]],{i,nPar}],1/\[Sigma]]*(*If[d>1,1/Product[\[Sigma][[i]],{i,d}],1/\[Sigma]],1/\[Sigma]*)(KGausNorm[(\[Eta]-\[Eta]Mean)/\[Sigma]]+KGausNorm[(\[Eta]+\[Eta]Mean-2\[Eta]Min)/\[Sigma]]+KGausNorm[(2\[Eta]Max-\[Eta]-\[Eta]Mean)/\[Sigma]]);
# {1/ NSample Sum[BinnedDataTab[[i,2]]*KGausBound[BinnedDataTab[[i,1]],StepTab/2  ],{i,nBins}],Interpolation[Partition[BinnedDataTab//Flatten,nPar+1]]}
# ];
#
#
# fKernelBinned2D[\[Eta]_,DataTab_,\[Eta]Min_,\[Eta]Max_,nSteps_,nExtraSteps_]:=Module[
# {NSample,nPar=2,nBins,x1DataRange,x1Step,x2DataRange,x2Step,CountsTab,MaxCount,CentersTab,CentersAndCountsTab,KGausNorm,KGausBound,\[Eta]Mean},
#
# NSample=Length[DataTab];
# x1DataRange={Min[DataTab[[All,1]]],Max[DataTab[[All,1]]]};
# x1Step=(x1DataRange[[2]]-x1DataRange[[1]])/nSteps[[1]];
# x2DataRange={Min[DataTab[[All,2]]],Max[DataTab[[All,2]]]};
# x2Step=(x2DataRange[[2]]-x2DataRange[[1]])/nSteps[[2]];
# CountsTab=BinCounts[DataTab,{x1DataRange[[1]]-nExtraSteps[[1]]*x1Step,x1DataRange[[2]]+nExtraSteps[[1]]*x1Step,x1Step},{x2DataRange[[1]]-nExtraSteps[[2]]*x2Step,x2DataRange[[2]]+nExtraSteps[[2]]*x2Step,x2Step}];
# MaxCount=Max[CountsTab];CentersTab=Table[{i,j},{i,x1DataRange[[1]]-(nExtraSteps[[1]]-1/2)*x1Step,x1DataRange[[2]]+(nExtraSteps[[1]]-1/2)*x1Step,x1Step},{j,x2DataRange[[1]]-(nExtraSteps[[2]]-1/2)*x2Step,x2DataRange[[2]]+(nExtraSteps[[2]]-1/2)*x2Step,x2Step}];
# CentersAndCountsTab=Partition[Table[{CentersTab[[i,j]],CountsTab[[i,j]]/MaxCount}//Flatten,{i,nSteps[[1]]+2nExtraSteps[[1]]},{j,nSteps[[2]]+2nExtraSteps[[2]]}]//Flatten,3];
# nBins=Length[CentersAndCountsTab];
#
# KGausNorm[x_]:=1/(2\[Pi])^(nPar/2) Exp[-(x.x/2)];
# KGausBound[\[Eta]Mean_,\[Sigma]_]:=1/(\[Sigma][[1]]*\[Sigma][[2]])*(KGausNorm[(\[Eta]-\[Eta]Mean)/\[Sigma]]+KGausNorm[(\[Eta]+\[Eta]Mean-2\[Eta]Min)/\[Sigma]]+KGausNorm[(2\[Eta]Max-\[Eta]-\[Eta]Mean)/\[Sigma]]);
# 1/ NSample Sum[CentersAndCountsTab[[i,3]]*(KGausBound[CentersAndCountsTab[[i,{1,2}]],{x1Step,x2Step}] ),{i,nBins}]
# (*
# KGausBound[\[Eta]Mean_,]:=1/{x1Step,x2Step}*(KGausNorm[(\[Eta]-\[Eta]Mean)/{x1Step,x2Step}]+KGausNorm[(\[Eta]+\[Eta]Mean-2\[Eta]Min)/{x1Step,x2Step}]+KGausNorm[(2\[Eta]Max-\[Eta]-\[Eta]Mean)/{x1Step,x2Step}]);
# 1/ NSample Sum[CentersAndCountsTab[[i,3]]*KGausBound[CentersAndCountsTab[[i,{1,2}]]  ],{i,nBins}]
# *)
# ];
#
#
#
#
#
# End[];
#
#
# EndPackage[];
