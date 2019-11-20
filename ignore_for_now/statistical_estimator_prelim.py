# (* ::Package:: *)
#
# BeginPackage["StatisticalEstimatorsv01`"];
#
#
# CBI::usage = "CBI[{x1,x2,...}] returns the bi-weight central estimator from Beers et al.";
# SBI::usage = "SBI[{x1,x2,...}] returns the bi-weight dispersion from Beers et al.";
#
# BCES96::usage = "Linear regression following Akritas & Bershady (1996).
# BCES96[{{Y1,\[Delta]Y1,Y2,\[Delta]Y2,\[Delta]Y1Y2},...}]";
#
# WeightedMean::usage="";
# f\[CapitalDelta]NGherels::usage="";
#
# FHarmonic::usage="harmonic radius";
# FJackknife::usage="...";
#
# FClipping::usage="clipping mean; Clipping procedure to cut outliers.
# Data of interest are in the nCol-th column of Data.
# The procedure cut away data that differ from more than nS dispersions,
# calculated through the S estimator, from the best estimator evaluated with C";
# 
# FGapZBH::usage="";
# FShiftGapper::usage="";
# \[Delta]Dressler::usage="";
# \[CapitalDelta]DresslerBoot::usage="";
#
# F\[CapitalDelta]RCutInAnnuli::usage="";
# F\[CapitalDelta]RCutInSquare::usage="";
# FRadialBinning::usage="";
#
# fStep::usage="";
# fKSTest1::usage="";
# fKSTest2::usage="";
#
# \[Mu]::usage="";
# fEllip::usage="";
# fEllipticity::usage="";
#
# fAnnuli::usage="";
#
# fFrom\[Alpha]\[Delta]To\[CapitalDelta]::usage="";
#
# KGaus::usage="";
# KGaus2::usage="";
# hSmooth::usage="";
# hSilverman::usage="";
# KRefBound::usage="";
# fRefBound::usage="";
# KRef0Inf::usage="";
# fRef0Inf::usage="";
# KRef::usage="";
# fRef::usage="";
# fka::usage="";
# Mfka::usage="";
# MfkaMin::usage="";
# fMainClump::usage="";
# fkazFinder::usage="";
#
# fRandom::usage="";
# fBinnedList2D::usage="";
#
# fRunNotebook::usage="";
#
# FResamData::usage="";
#
#
# Begin["`Private`"]
#
#
# fRunNotebook[FileName_]:=Module[{nb},
# nb=NotebookOpen[FileName];
# SelectionMove[nb,All,Notebook];
# SelectionEvaluate[nb];
# NotebookClose[nb];
# ];
#
#
# WeightedMean[DataTab_]:=Block[{nData=Length[DataTab],\[Sigma]WeithMean},
# \[Sigma]WeithMean=Total[1./DataTab[[All,2]]^2.  ]^(-0.5);
# {\[Sigma]WeithMean^2. Total[DataTab[[All,1]]/DataTab[[All,2]]^2.],\[Sigma]WeithMean}
# ];
#
#
# f\[CapitalDelta]NGherels[n_]:=
# Which[n==0,{0,1.841},n==1,{0.173,3.3},n==2,{0.708,4.638},n==3,{1.367,5.918},n==4,{2.086,7.163},n==5,{2.840,8.382},n==6,{3.620,9.584},n==7,{4.419,10.77},n==8,{5.232,11.95},n==9,{6.057,13.11},n==10,{6.891,14.27},n==11,{7.734,15.42},n==12,{8.585,16.56},n==13,{9.441,17.70},n==14,{10.30,18.83},n==15,{11.17,19.96}];
#
#
# CBI[Data_] :=
#   Module[{CBITmp1, CBITab, DataTmp1, zMedian, zMAD, uTab,\[CapitalDelta]uTab,\[CapitalDelta]uTabTmp2,zBINumTmp1,zBIDenTmp1, nDataTmp1},
#    CBITab = {};
#    Do[
#     DataTmp1 = If[Length[Data[[1]]] == 0, Data, Data[[All, j]]];
#     zMedian = Median[DataTmp1];
#     nDataTmp1 = Length[DataTmp1];
#     zMAD = MedianDeviation[DataTmp1];
#     If[zMAD > 0,
# \[CapitalDelta]uTab=(DataTmp1-zMedian);
# uTab=\[CapitalDelta]uTab/(6.*zMAD);
# \[CapitalDelta]uTabTmp2= UnitStep[1.-Abs[uTab]]*(1.-uTab^2)^2;
# zBINumTmp1=Total[\[CapitalDelta]uTab*\[CapitalDelta]uTabTmp2];
# zBIDenTmp1=Total[\[CapitalDelta]uTabTmp2];
# CBITmp1=zMedian+zBINumTmp1/zBIDenTmp1,
# CBITmp1=zMedian];
# AppendTo[CBITab, CBITmp1],
# {j, Max[1, Length[Data[[1]]] ]     }];If[Length[Data[[1]]] == 0, CBITab[[1]], CBITab]];
#
#
#
# SBI[Data_]:=
# Block[{nColumns,SBITmp1,SBITab,DataTmp1,vMedian,vMAD,nData,uTab,\[CapitalDelta]uTab,uTabTmp2,uTabTmp3,SBIDenTmp1,SBINumTmp1},
# nColumns=Length[Data[[1]]];
# SBITab={};
# Do[
# DataTmp1=If[nColumns==0,Data,Data[[All,j]]];
# vMedian=Median[DataTmp1];
# vMAD=MedianDeviation[DataTmp1];
# If[vMAD>0.,
# nData=Length[DataTmp1];
# \[CapitalDelta]uTab=(DataTmp1-vMedian);
# uTab=\[CapitalDelta]uTab/(9.* vMAD);
# uTabTmp2=UnitStep[1.-Abs[uTab]];
# uTabTmp3=(1.-uTab^2);
# SBIDenTmp1=Total[uTabTmp2*uTabTmp3*(1.-5.*uTab^2)];SBINumTmp1=Sqrt[Total[uTabTmp2*\[CapitalDelta]uTab^2 *uTabTmp3^4]];SBITmp1=Sqrt[nData]* SBINumTmp1/Abs[SBIDenTmp1],
# SBITmp1=0.];
# AppendTo[SBITab,SBITmp1],
# {j,1,Max[1,nColumns]}];If[nColumns==0,Return[SBITab[[1]]],Return[SBITab]]];
#
#
# Options[BCES96]={Scatter->"weighted"(*"unweighted"*)};
# BCES96[Data_,OptionsPattern[]]:=Block[
# {Y1Tab=Data[[All,1]],Y2Tab=Data[[All,3]],\[Delta]Y1Tab=Data[[All,2]],\[Delta]Y2Tab=Data[[All,4]],
# nData=Length[Data],FromSampleToPopulationVariance=(nData-1)/nData,
# Y1Mean,Y2Mean,\[Alpha]Tab,\[Delta]\[Alpha]Tab,\[Beta]Tab,\[Beta]1,\[Beta]2,\[Beta]3,\[Beta]4,\[Epsilon]Tab,\[Delta]\[Beta]Tab,
# SY1Y1,SY2Y2,SY1Y2,\[Delta]Y1Y1Mean,\[Delta]Y2Y2Mean,\[Delta]Y1Y2Mean,\[Delta]Y1Y2Tab,
# \[Xi]1Tab,\[Xi]2Tab,\[Xi]3Tab,\[Xi]4Tab,\[Xi]Tab,\[Zeta]Tab,
# OptionValueScatter,
# SigmaStatTab,wTab,wTabNorm,SigmaIntSq},
# (*Data={{Y1,\[Delta]Y1,Y2,\[Delta]Y2,\[Delta]Y1Y2}}*)
# (* X2=\[Alpha]1+\[Beta]1*X1+\[Epsilon] *)
#
# OptionValueScatter=OptionValue[Scatter];
#
# \[Delta]Y1Y2Tab=If[Length[Data[[1]]]==5,Data[[All,5]],ConstantArray[0.,nData]];
#
# Y1Mean=Mean[Y1Tab];
# Y2Mean=Mean[Y2Tab];
# SY1Y1=FromSampleToPopulationVariance* Variance[Y1Tab] (*Variance is the sample variance ->1/(n-1)*);
# SY2Y2=FromSampleToPopulationVariance Variance[Y2Tab];
# SY1Y2=FromSampleToPopulationVariance Covariance[Y1Tab,Y2Tab];
#
# \[Delta]Y1Y1Mean=Mean[\[Delta]Y1Tab^2];
# \[Delta]Y2Y2Mean=Mean[\[Delta]Y2Tab^2];
# \[Delta]Y1Y2Mean=Mean[\[Delta]Y1Y2Tab];
#
# (*Eq .9*)
# \[Beta]1(*BCES(X2|x1)*)=(SY1Y2(*covar_y1y2*)-\[Delta]Y1Y2Mean(*sig12var*))/(SY1Y1(*y1var*)-\[Delta]Y1Y1Mean(*sig11var*));
#
# (*Eq .23, Subscript[\[CapitalSigma], ij,n] is Subscript[V, ij,n]*)
# \[Beta]2(*BCES(X1|x2)*)=(SY2Y2-\[Delta]Y2Y2Mean)/(SY1Y2-\[Delta]Y1Y2Mean);
#
# \[Beta]3(*bisector*)=(\[Beta]1*\[Beta]2-1.+Sqrt[(1.+\[Beta]1^2)(1.+\[Beta]2^2)])/(\[Beta]1+\[Beta]2);
# \[Beta]4(*orthogonal*)=0.5((\[Beta]2-1./\[Beta]1)+Sign[SY1Y2]Sqrt[4.+(\[Beta]2-1./\[Beta]1)^2]);
#
# \[Beta]Tab={\[Beta]1,\[Beta]2,\[Beta]3,\[Beta]4};
#
# \[Alpha]Tab=Y2Mean-{\[Beta]1,\[Beta]2,\[Beta]3,\[Beta]4}*Y1Mean;
#
# (*Eq .11, sample variances are menat to be unbiased (i.e. .../n)*)
# \[Xi]1Tab=Table[((Y1Tab[[i]]-Y1Mean)*(Y2Tab[[i]]-\[Beta]1*Y1Tab[[i]]-\[Alpha]Tab[[1]])+\[Beta]1*(\[Delta]Y1Tab[[i]]^2)-\[Delta]Y1Y2Tab[[i]](*questo termine non c'\[EGrave] in .f*))/(SY1Y1-\[Delta]Y1Y1Mean),{i,nData}];
# (*Eq .24*)
# \[Xi]2Tab=Table[((Y2Tab[[i]]-Y2Mean)*(Y2Tab[[i]]-\[Beta]2*Y1Tab[[i]]-\[Alpha]Tab[[2]])+\[Beta]2*\[Delta]Y1Y2Tab[[i]](*non c'\[EGrave] nel codice*)-\[Delta]Y2Tab[[i]]^2)/(SY1Y2(*text -> SY1Y2/FromSampleToPopulationVariance*)-\[Delta]Y1Y2Mean(*not in .f*)),{i,nData}];
# \[Xi]3Tab=\[Beta]3/((\[Beta]1+\[Beta]2)Sqrt[(1.+\[Beta]1^2)(1.+\[Beta]2^2)])*((1.+\[Beta]2^2)\[Xi]1Tab+(1.+\[Beta]1^2)\[Xi]2Tab);
# (*\[Xi]3Tab=((1.+\[Beta]2^2)\[Beta]3)/((\[Beta]1+\[Beta]2)Sqrt[(1+\[Beta]1^2(* .f->\[Beta]1^2, text->\[Beta]2^1*))(1.+\[Beta]2^2)])*\[Xi]1Tab+((1.+\[Beta]1^2)\[Beta]3)/((\[Beta]1+\[Beta]2)Sqrt[(1.+\[Beta]1^2)(1+\[Beta]2^2)])*\[Xi]2Tab;*)
# \[Xi]4Tab=\[Beta]4/Sqrt[4.+(\[Beta]2-(1/\[Beta]1))^2]*(\[Xi]1Tab/\[Beta]1^2+\[Xi]2Tab);
# (*\[Xi]4Tab=\[Beta]4/(\[Beta]1^2 Sqrt[4.+(\[Beta]2-(1/\[Beta]1))^2])*\[Xi]1Tab+\[Beta]4/Sqrt[4.+(\[Beta]2-\[Beta]1^-1)^2]*\[Xi]2Tab;*)
#
# \[Xi]Tab={\[Xi]1Tab,\[Xi]2Tab,\[Xi]3Tab,\[Xi]4Tab};
# \[Zeta]Tab=Table[Y2Tab[[i]]-{\[Beta]1,\[Beta]2,\[Beta]3,\[Beta]4}*Y1Tab[[i]]-Y1Mean*\[Xi]Tab[[{1,2,3,4},i]],{i,nData}];
# (*
# \[Zeta]1Tab=Table[Y2Tab[[i]]-\[Beta]1*Y1Tab[[i]]-Y1Mean*\[Xi]1Tab[[i]],{i,nData}];
# *)
# \[Delta]\[Beta]Tab=Table[(1/nData FromSampleToPopulationVariance*Variance[\[Xi]Tab[[i]]])^0.5,{i,4}];
# \[Delta]\[Alpha]Tab=Table[(1/nData FromSampleToPopulationVariance*Variance[\[Zeta]Tab[[All,i]]])^0.5,{i,4}];
# (*
# Do[
# \[Sigma]\[Beta]Squared=FromSampleToPopulationVariance*Variance[\[Xi]Tab[[i]]];
# (*variance of Subscript[\[Beta], 1]*)
# \[Delta]\[Beta][i]=Sqrt[\[Sigma]\[Beta]Squared/nData];
# \[Sigma]\[Alpha]Squared=FromSampleToPopulationVariance*Variance[\[Zeta]Tab[[All,i]]];
# \[Delta]\[Alpha][i]=Sqrt[\[Sigma]\[Alpha]Squared/nData];
# ,{i,1,4}];
# *)
#
# (*
# Cov\[Beta]1\[Beta]2=(\[Beta]1*(nData*(SY1Y1-\[Delta]Y1Y1Mean))^2)^-1 Sum[(Y1Tab[[i]]-Y1Mean)*(Y2Tab[[i]]-Y2Mean)(Y2Tab[[i]]-Y2Mean-\[Beta]1(Y1Tab[[i]]-Y1Mean))(Y2Tab[[i]]-Y2Mean-\[Beta]2(Y1Tab[[i]]-Y1Mean)),{i,nData}];
# \[Delta]\[Beta]3=(\[Beta]3^2/((\[Beta]1+\[Beta]2)^2 (1.+\[Beta]1^2)(1.+\[Beta]2^2)) ((1.+\[Beta]2^2)^2*\[Delta]\[Beta]1^2+2.(1.+\[Beta]1^2)(1+\[Beta]2^2)Cov\[Beta]1\[Beta]2+(1.+\[Beta]1^2)^2 \[Delta]\[Beta]2^2))^0.5;
# \[Delta]\[Beta]4=(\[Beta]4^2/(4.\[Beta]1^2+(\[Beta]1*\[Beta]2-1.)^2) (\[Beta]1^-2*\[Delta]\[Beta]1^2+2.Cov\[Beta]1\[Beta]2+\[Beta]1^2 \[Delta]\[Beta]2^2))^0.5;
# *)
#
# (*intrinsic scatter after Kelly, Apj 665:1489\[Dash]1506 (2007), \[Section]7 .1, pag 1499*)
# \[Epsilon]Tab=SY1Y1-\[Delta]Y1Y1Mean-\[Beta]Tab*SY1Y2;
#
# (*intrinsic scatter after planck early XI, A&A 536, A11 (2011), pratt et al. 2009*)
# SigmaStatTab=Table[\[Delta]Y2Tab^2+\[Beta]Tab[[i]]^2*\[Delta]Y1Tab^2,{i,Length[\[Beta]Tab]}];
#
# Which[
# OptionValueScatter=="weighted",
# wTabNorm=Table[(1./nData)Total[1/SigmaStatTab[[Ind\[Beta]]]],{Ind\[Beta],Length[\[Beta]Tab]}];
# wTab=Table[(1./SigmaStatTab[[Ind\[Beta],IndData]])/wTabNorm[[Ind\[Beta]]],{Ind\[Beta],Length[\[Beta]Tab]},{IndData,nData}],
# OptionValueScatter=="unweighted",
# wTab=Table[ConstantArray[1,nData],{Ind\[Beta],Length[\[Beta]Tab]},{IndData,nData}]
# ];
#
# (*unweighted scatter in Foex et al. 1208.4026*)
#
# SigmaIntSq=1./(nData-2.) Table[
# Sum[wTab[[Ind\[Alpha],IndData]]*(Max[(Y2Tab[[IndData]]-\[Beta]Tab[[Ind\[Alpha]]]*Y1Tab[[IndData]]-\[Alpha]Tab[[Ind\[Alpha]]])^2
# -(nData-2)/nData SigmaStatTab[[Ind\[Alpha],IndData]],0.]),{IndData,nData}],
# {Ind\[Alpha],Length[\[Alpha]Tab]}];
#
#
# {{"","\[Alpha]","\[Delta]\[Alpha]","\[Beta]","\[Delta]\[Beta]","\[Sigma]1","\[Sigma]2"},
# {"BCES(X2|X1)",\[Alpha]Tab[[1]],\[Delta]\[Alpha]Tab[[1]],\[Beta]1,\[Delta]\[Beta]Tab[[1]],Max[SigmaIntSq[[1]],0.]^0.5,(Max[\[Epsilon]Tab[[1]],0.])^0.5},
# {"BCES(X1|X2)",\[Alpha]Tab[[2]],\[Delta]\[Alpha]Tab[[2]],\[Beta]2,\[Delta]\[Beta]Tab[[2]],(Max[SigmaIntSq[[2]],0.])^0.5,Max[\[Epsilon]Tab[[2]],0.]^0.5},
# {"BCES bisector",\[Alpha]Tab[[3]],\[Delta]\[Alpha]Tab[[3]],\[Beta]3,\[Delta]\[Beta]Tab[[3]],(Max[SigmaIntSq[[3]],0.])^0.5,Max[\[Epsilon]Tab[[3]],0.]^0.5},
# {"BCES orthogonal",\[Alpha]Tab[[4]],\[Delta]\[Alpha]Tab[[4]],\[Beta]4,\[Delta]\[Beta]Tab[[4]],(Max[SigmaIntSq[[4]],0.])^0.5,Max[\[Epsilon]Tab[[4]],0.]^0.5}
# }
# ];
#
#
# FClipping[Data_,nCol_,C_,S_,nS_]:=Module[{DataClippedTmp1,MeanDataClippedTmp1,xiFirstTmp1,xiLastTmp1,xiClippedTmp1,DataCippledFin},
# DataClippedTmp1=Sort[Data,#1[[nCol]]<#2[[nCol]]&];
# {MeanDataClippedTmp1,xiFirstTmp1,xiLastTmp1}={C[DataClippedTmp1[[All,nCol]]],Take[DataClippedTmp1,1][[1]],Take[DataClippedTmp1,-1][[1]]};
# If[
# Abs[MeanDataClippedTmp1-xiFirstTmp1[[nCol]]]>Abs[xiLastTmp1[[nCol]]-MeanDataClippedTmp1],
# DataClippedTmp1=Drop[DataClippedTmp1,1];xiClippedTmp1=xiFirstTmp1,
# DataClippedTmp1=Drop[DataClippedTmp1,-1];xiClippedTmp1=xiLastTmp1];
# While[
# Abs[  C[DataClippedTmp1[[All,nCol]]]-xiClippedTmp1[[nCol]]  ]>nS*S[DataClippedTmp1[[All,nCol]]],
# {MeanDataClippedTmp1,xiFirstTmp1,xiLastTmp1}={Mean[DataClippedTmp1[[All,nCol]]],Take[DataClippedTmp1,1][[1]],Take[DataClippedTmp1,-1][[1]]};
# If[
# Abs[MeanDataClippedTmp1-xiFirstTmp1[[nCol]]]>Abs[xiLastTmp1[[nCol]]-MeanDataClippedTmp1],
# DataClippedTmp1=Drop[DataClippedTmp1,1];xiClippedTmp1=xiFirstTmp1,
# DataClippedTmp1=Drop[DataClippedTmp1,-1];xiClippedTmp1=xiLastTmp1
# ];
# ];
# DataCippledFin=Sort[Join[DataClippedTmp1,{xiClippedTmp1}]];
# {DataCippledFin,C[DataCippledFin[[All,nCol]]],S[DataCippledFin[[All,nCol]]]}
# ]
#
#
# FResamData[pBoot_,xMin_,xMax_,nSample_,nBoot_]:=Module[{yMaxv,i,iTot,BootSample,xv,yv,pv},
#
# yMaxv=-FindMinimum[{-pBoot[x],xMin<=x<=xMax},{x,(xMin+xMax)/2}][[1]];
#
# For[
# i=iTot=0;BootSample={},i<nSample*nBoot,
# iTot++,
# {xv,yv}={  Random[Real,{xMin,xMax}],yMaxv*Random[]   };
# pv=pBoot[xv];
# If[yv<pv,AppendTo[BootSample,{xv,yv}];i++]
# ];
# {Partition[BootSample[[All,1]],nSample],i/iTot}
# ];
#
#
# (* ::Subsubsection:: *)
# (*Jack Knife*)
#
#
# FJackknife[Data_,FOnData_,NParFOnData_,ParFOnData_]:=(
# NDataTmp1=Length[Data];
# Which[
# NParFOnData==0,yFTmp1[DataJacknifed_]:=FOnData[DataJacknifed],
# NParFOnData==1,yFTmp1[DataJacknifed_]:=FOnData[DataJacknifed,ParFOnData[[1]]] ,
# NParFOnData==2,yFTmp1[DataJacknifed_]:=FOnData[DataJacknifed,ParFOnData[[1]],ParFOnData[[2]]]
# ];
# yAllTmp1=yFTmp1[Data];
# yTabTmp1={};
# Do[
# DataJacknifedTmp1=Drop[Data,{i}];
# yTmp1=yFTmp1[DataJacknifedTmp1];
# AppendTo[yTabTmp1,NDataTmp1*yAllTmp1-(NDataTmp1-1)yTmp1],
# {i,1,NDataTmp1}];
# {Mean[yTabTmp1],Sqrt[StandardDeviation[yTabTmp1]]}
# );
#
#
# (*Subscript[R, PV]=N (N-1)/Underscript[\[Sum], i>j]Subscript[R, ij]^-1*)
# FHarmonic[Data_,xCol_,yCol_]:=(
# NTmp1=Length[Data];
# NTmp1(NTmp1-1)(Sum[Sum[1/(\[Sqrt]((Data[[i,xCol]]-Data[[j,xCol]])^2+(Data[[i,yCol]]-Data[[j,yCol]])^2)),{i,j+1,NTmp1}],{j,1,NTmp1-1}])^-1)
#
#
# (* ::Section:: *)
# (*Gapper*)
#
#
# FGapZBH[Data_,nCol_,gap_]:=(
# DataGapTmp1=Sort[Data,#1[[nCol]]<#2[[nCol]]&];
# NvTmp1=Length[DataGapTmp1];
# iMedianDataGapTmp1=Round[NvTmp1/2];
# For[
# iSupTmp1=iMedianDataGapTmp1+1,
# iSupTmp1 <=NvTmp1 && DataGapTmp1[[iSupTmp1,nCol]]-DataGapTmp1[[iSupTmp1-1,nCol]]<gap ,
# iSupTmp1++ (*the last Subscript[i, sup] to be computed fails the test, Subscript[i, sup]-1 is the last one to pass it*)
# ];
# For[
# iMinTmp1=iMedianDataGapTmp1-1,
# iMinTmp1 >=1 && DataGapTmp1[[iMinTmp1+1,nCol]]-DataGapTmp1[[iMinTmp1,nCol]]<gap ,
# iMinTmp1--
# ];
# DataGapFin=Take[DataGapTmp1,{iMinTmp1+1,iSupTmp1-1}];
# (*{DataGapFin,Mean[DataGapFin[[All,nCol]]],StandardDeviation[DataGapFin[[All,nCol]]]}*)
# DataGapFin
# );
#
#
# FShiftGapper[Data_,xCol_,yCol_,nCol_,\[CapitalDelta]Rv_,nMin_,Gap_]:=(
# VelRadTab=Sort[Table[{Sqrt[Data[[i,xCol]]^2+Data[[i,yCol]]^2],Data[[i]]}//Flatten,{i,1,Length[Data]}],#1[[1]]<#2[[1]]&];
# RMaxv=VelRadTab[[Length[VelRadTab],1]];
#
# RadSecTab={};
# RInv=0;
# iLastTmp1=1;
# While[
# RInv<=RMaxv && iLastTmp1<=Length[Data],
# RadSecTabElTmp1=Select[VelRadTab,(RInv<=#[[1]]< RInv+\[CapitalDelta]Rv)&];
# RInv+=\[CapitalDelta]Rv;
# iLastTmp1=Position[VelRadTab,RadSecTabElTmp1[[Length[RadSecTabElTmp1]]]][[1,1]];
# While[
# Length[RadSecTabElTmp1]<nMin && iLastTmp1<Length[Data],
# iLastTmp1++; AppendTo[RadSecTabElTmp1, VelRadTab[[iLastTmp1]]  ];RInv=If[iLastTmp1<Length[Data],VelRadTab[[iLastTmp1+1,1]],VelRadTab[[iLastTmp1,1]]+\[CapitalDelta]Rv ]
#  ];
# AppendTo[RadSecTab,RadSecTabElTmp1];
# ];
#
# If[
# Length[RadSecTab]>2,
# If[
# Length[   RadSecTab[[  Length[RadSecTab]   ]]   ]<nMin,
# RadSecTab=Join[
# Table[RadSecTab[[i]],{i,1,Length[RadSecTab]-2}],{Join[ RadSecTab[[Length[RadSecTab]-1]],RadSecTab[[Length[RadSecTab] ]] ] }
# ];
# ]
# ];
#
# CutRadSecTab={};
# Do[
# AppendTo[CutRadSecTab,FGapZBH[RadSecTab[[i]],nCol+1,Gap] ],
# {i,1,Length[RadSecTab]}
# ];
# CutRadSecTabTmp1=Partition[CutRadSecTab//Flatten,Length[CutRadSecTab[[1,1]]]];
# vCut3TabTmp3=CutRadSecTabTmp1[[All,Range[2,Length[CutRadSecTabTmp1[[1]] ]  ]   ]];
# {vCut3TabTmp3,
# Table[{RadSecTab[[i,1,1]],RadSecTab[[i,Length[RadSecTab[[i]]],1]],Length[RadSecTab[[i]] ]},{i,1,Length[RadSecTab]}]}
# );
#
#
# (* ::Subsubsection:: *)
# (*Substructures*)
#
#
# \[Delta]Dressler[Data_,iPos_,xPos_,yPos_,zPos_,nLoc_,C_,S_]:=(
# NDataTmp1=Length[Data];
# NeighTabTmp1={};
# Do[
# \[CapitalDelta]RTabTmp1=
# Table[{
# j,\[Sqrt]((Data[[j,xPos]]-Data[[i,xPos]])^2+(Data[[j,yPos]]-Data[[i,yPos]])^2)},{j,1,NDataTmp1}];
# \[CapitalDelta]RTabTmp2=Take[Sort[\[CapitalDelta]RTabTmp1,#1[[2]]<#2[[2]]&],nLoc];
# AppendTo[NeighTabTmp1,\[CapitalDelta]RTabTmp2[[All,1]]],
# {i,1,NDataTmp1}
# ];
#
# zGloTmp1=C[Data[[All,zPos]] ];
# \[Sigma]GloTmp1=S[Data[[All,zPos]] ];
#
# tDist1\[Sigma]Tmp1=Abs[FindRoot[CDF[StudentTDistribution[nLoc-1],x]-((1-CI)/2/.{CI->0.6826894921370859`}),{x,1}][[1,2]]];
# ChiSquareMDist1\[Sigma]Tmp1=Abs[FindRoot[CDF[ChiSquareDistribution[nLoc-1],x]-((1-CI)/2/.{CI->0.6826894921370859`}),{x,1}][[1,2]]];
# ChiSquarePDist1\[Sigma]Tmp1=Abs[FindRoot[CDF[ChiSquareDistribution[nLoc-1],x]-((1+CI)/2/.{CI->0.6826894921370859`}),{x,1}][[1,2]]];
#
# \[Delta]TabTmp1={};
# Do[
# zLocTmp1=C[Data[[All,zPos]][[ NeighTabTmp1[[i]] ]] ];
# \[Sigma]LocTmp1=S[Data[[All,zPos]][[ NeighTabTmp1[[i]] ]] ];
# \[Delta]BKTTmp1=1/\[Sigma]GloTmp1 \[Sqrt](((zLocTmp1-zGloTmp1)/(tDist1\[Sigma]Tmp1/Sqrt[nLoc]))^2+(Max[(\[Sigma]LocTmp1-\[Sigma]GloTmp1),0]/(((nLoc-1)/ChiSquareMDist1\[Sigma]Tmp1)^(1/2)-((nLoc-1)/ChiSquarePDist1\[Sigma]Tmp1)^(1/2)))^2);
# \[Delta]DSTmp1=Sqrt[nLoc/\[Sigma]GloTmp1^2] Sqrt[(zLocTmp1-zGloTmp1)^2+(Max[(\[Sigma]LocTmp1-\[Sigma]GloTmp1),0])^2];
# AppendTo[\[Delta]TabTmp1,{Data[[i]],{\[Delta]BKTTmp1,\[Delta]DSTmp1}}]
# ,{i,1,NDataTmp1}
# ];
#
# {\[Delta]TabTmp1,Sum[\[Delta]TabTmp1[[i,-1]],{i,1,Length[\[Delta]TabTmp1]}],NeighTabTmp1}
# );
#
#
# \[CapitalDelta]DresslerBoot[Data_,iPos_,xPos_,yPos_,zPos_,nLoc_,C_,S_,nBoot_]:=(
# \[CapitalDelta]BootTabTmp1={};
# \[Delta]BKTBootTabTmp1={};
# \[Delta]DSBootTabTmp1={};
# Do[
# PosBootTabTmp1=RandomSample[Data[[All,{xPos,yPos}]] ];
# zBootTabTmp1=RandomSample[Data[[All,zPos]] ];
# DataBootTmp1=Table[{j,PosBootTabTmp1[[j]],zBootTabTmp1[[j]]  }//Flatten,{j,1,Length[Data]}];
# \[Delta]DresslerTmp1=\[Delta]Dressler[DataBootTmp1,1,2,3,4,nLoc,C,S];
# AppendTo[\[CapitalDelta]BootTabTmp1,\[Delta]DresslerTmp1[[2]] ];
# AppendTo[\[Delta]BKTBootTabTmp1,\[Delta]DresslerTmp1[[1]][[All,2,1]]  ];
# AppendTo[\[Delta]DSBootTabTmp1,\[Delta]DresslerTmp1[[1]][[All,2,2]]  ],
# {i,1,nBoot}];
# {\[CapitalDelta]BootTabTmp1,\[Delta]BKTBootTabTmp1//Flatten,\[Delta]DSBootTabTmp1//Flatten}
# );
#
#
# (*
# \[CapitalDelta]DresslerBoot[Data_,iPos_,xPos_,yPos_,zPos_,nLoc_,C_,S_,nBoot_]:=(
# \[CapitalDelta]BootTabTmp1={};
# \[Delta]BKTBootTabTmp1={};
# \[Delta]DSBootTabTmp1={};
# Do[
# PosBootTabTmp1=RandomSample[Data[[All,{xPos,yPos}]] ];
# zBootTabTmp1=RandomSample[Data[[All,zPos]] ];
# DataBootTmp1=Table[{j,PosBootTabTmp1[[j]],zBootTabTmp1[[j]]  }//Flatten,{j,1,Length[Data]}];
# \[Delta]DresslerTmp1=\[Delta]Dressler[DataBootTmp1,1,2,3,4,nLoc,C,S];
# AppendTo[\[CapitalDelta]BootTabTmp1,\[Delta]DresslerTmp1[[{2}]] ];
# AppendTo[\[Delta]BKTBootTabTmp1,\[Delta]DresslerTmp1[[1]][[All,-2]] ];
# AppendTo[\[Delta]DSBootTabTmp1,\[Delta]DresslerTmp1[[1]][[All,-1]] ],
# {i,1,nBoot}];
# {\[CapitalDelta]BootTabTmp1,\[Delta]BKTBootTabTmp1//Flatten,\[Delta]DSBootTabTmp1//Flatten}
# );
# *)
#
#
# (* ::Section:: *)
# (*Cuts*)
#
#
# (* ::Text:: *)
# (*Galaxies and sampled positions inside squared or circular regions*)
#
#
# F\[CapitalDelta]RCutInAnnuli[Data_,xPos_,yPos_,RMin_,RMax_]:=(
# Select[Data,RMin<= Sqrt[#1[[xPos]]^2+ #1[[yPos]]^2]<=RMax&]
# );
#
# F\[CapitalDelta]RCutInSquare[Data_,xPos_,yPos_,RMin_,RMax_]:=(
# Select[Data,RMin<=Abs[ #1[[xPos]]]<=RMax && RMin<=Abs[ #1[[yPos]]]<=RMax&]
# );
#
#
# (* ::Subsubsection:: *)
# (*Radial binning*)
#
#
# FRadialBinning[Data_,xCol_,yCol_,\[CapitalDelta]Rv_,nMin_]:=(
# VelRadTab=Sort[Table[{Sqrt[Data[[i,xCol]]^2+Data[[i,yCol]]^2],Data[[i]]}//Flatten,{i,1,Length[Data]}],#1[[1]]<#2[[1]]&];
# RMaxv=VelRadTab[[Length[VelRadTab],1]];
#
# RadSecTab={};
# RInv=0;
# iLastTmp1=1;
# While[
# RInv<=RMaxv && iLastTmp1<=Length[Data],
# RadSecTabElTmp1=Select[VelRadTab,(RInv<=#[[1]]< RInv+\[CapitalDelta]Rv)&];
# RInv+=\[CapitalDelta]Rv;
# iLastTmp1=Position[VelRadTab,RadSecTabElTmp1[[Length[RadSecTabElTmp1]]]][[1,1]];
# While[
# Length[RadSecTabElTmp1]<nMin && iLastTmp1<Length[Data],
# iLastTmp1++; AppendTo[RadSecTabElTmp1, VelRadTab[[iLastTmp1]]  ];RInv=If[iLastTmp1<Length[Data],VelRadTab[[iLastTmp1+1,1]],VelRadTab[[iLastTmp1,1]]+\[CapitalDelta]Rv ]
#  ];
# AppendTo[RadSecTab,RadSecTabElTmp1];
# ];
#
# If[
# Length[RadSecTab]>2,
# If[
# Length[   RadSecTab[[  Length[RadSecTab]   ]]   ]<nMin,
# RadSecTab=Join[
# Table[RadSecTab[[i]],{i,1,Length[RadSecTab]-2}],{Join[ RadSecTab[[Length[RadSecTab]-1]],RadSecTab[[Length[RadSecTab] ]] ] }
# ];
# ]
# ];
#
# Table[{i,Mean[RadSecTab[[i]][[All,1]]],StandardDeviation[RadSecTab[[i]][[All,1]]],Length[RadSecTab[[i]] ],RadSecTab[[i,1,1]],RadSecTab[[i,Length[RadSecTab[[i]]],1]]},{i,1,Length[RadSecTab]}]
# );
#
#
# (* ::Section:: *)
# (*Kolmogorov-Smirnov*)
#
#
# fStep[x_,DataTab_]:=Module[{N1},
# N1=Length[DataTab];
# 1/N1 Sum[UnitStep[x-DataTab[[i]]],{i,1,N1}]//N
# ];
#
#
# fKSTest1[DataTab_,f_]:=Module[{N,DataSortTab,DKS,\[Lambda]KS},
#
# DataSortTab=Sort[DataTab];
# N=Length[DataTab];
#
# DKS=Max[Table[{ Abs[f[DataSortTab[[i]]]-(i-1)/N],Abs[i/N-f[DataSortTab[[i]]]]},{i,1,N}]//Flatten];
# \[Lambda]KS=(Sqrt[N]+0.12+0.11/Sqrt[N])*DKS;
# {DKS,2Sum[(-1)^(j-1) Exp[-2*j^2*\[Lambda]KS^2],{j,1,+\[Infinity]}]}//N
# ];
#
#
# fKSTest2[DataTab1_,DataTab2_]:=Module[{DataSortTab1,DataSortTab2,N1,N2,Ne,j1,j2,fn1,fn2,d1,d2,dt,DKS,\[Lambda]KS},
# DataSortTab1=Sort[DataTab1];
# DataSortTab2=Sort[DataTab2];
# N1=Length[DataTab1];
# N2=Length[DataTab2];
# Ne=(N1*N2)/(N1+N2);
#
# j1=j2=1;
# fn1=fn2=0;
# DKS=0;
#
# While[j1<=N1 &&j2<=N2,
# If[(d1=DataSortTab1[[j1]])<=(d2=DataSortTab2[[j2]]),fn1=(j1++)/N1];
# If[d2<=d1,fn2=(j2++)/N2];
# If[(dt=Abs[fn2-fn1])>DKS,DKS=dt]
# ];
# \[Lambda]KS=(Sqrt[Ne]+0.12+0.11/Sqrt[Ne])*DKS;
# {DKS,2Sum[(-1)^(j-1) Exp[-2 j^2 \[Lambda]KS^2],{j,1,+\[Infinity]}]}//N
# ];
#
#
# (*
# fECDF[DataTab_]:=Module[{N},
# N=Length[DataTab];
# Table[{DataTab[[i]],i/N},{i,1,Length}]
# ]
# *)
#
# (*
# fKSTest[DataTab1_,DataTab2_]:=Module[{N1,N2,Ne,fStep1,fStep2,DKS1,DKS2,\[Lambda]KS},
# N1=Length[DataTab1];
# N2=Length[DataTab2];
# Ne=(Length[DataTab1]*Length[DataTab2])/(Length[DataTab1]+Length[DataTab2]);
# fStep1[x_]:=fStep[x,DataTab1];
# fStep2[x_]:=fStep[x,DataTab2];
# DKS1=Max[ Table[{ Abs[    fStep1[ DataTab1[[i]]  ]-fStep2[ DataTab1[[i]]  ]     ] },{i,1,Length[DataTab1]}]];
# DKS2=Max[ Table[{ Abs[    fStep1[ DataTab2[[i]]  ]-fStep2[ DataTab2[[i]]  ]     ] },{i,1,Length[DataTab2]}]];
# \[Lambda]KS=(Sqrt[Ne]+0.12+0.11/Sqrt[Ne])*Max[{DKS1,DKS2}];
# {DKS1,DKS2,2Sum[(-1)^(j-1) Exp[-2 j^2 \[Lambda]KS^2],{j,1,+\[Infinity]}]}//N
# ];
# *)
#
#
# (*\[Alpha] is the R.A. in {hours, min, sec}; \[Delta] is the decliantion in {deg, arcmin, arcsec} *)
# fFrom\[Alpha]\[Delta]To\[CapitalDelta][\[Alpha]_,\[Delta]_,\[Alpha]0_,\[Delta]0_]:=Module[{\[Alpha]Deg,\[Delta]Deg,\[Alpha]0Deg,\[Delta]0Deg,DegToArcSec=60*60.,DegToRad=\[Pi]/180.,\[CapitalDelta]x1,\[CapitalDelta]x2},
# \[Alpha]Deg=15*(\[Alpha][[1]]+\[Alpha][[2]]/60+\[Alpha][[3]]/(60*60));
# \[Delta]Deg=Sign[\[Delta][[1]]](Abs[\[Delta][[1]]]+\[Delta][[2]]/60+\[Delta][[3]]/(60*60));
# \[Alpha]0Deg=15*(\[Alpha]0[[1]]+\[Alpha]0[[2]]/60+\[Alpha]0[[3]]/(60*60));
# \[Delta]0Deg=Sign[\[Delta]0[[1]]](Abs[\[Delta]0[[1]]]+\[Delta]0[[2]]/60+\[Delta]0[[3]]/(60*60));
# {\[CapitalDelta]x1,\[CapitalDelta]x2}=DegToArcSec*({\[Alpha]Deg,\[Delta]Deg}-{\[Alpha]0Deg,\[Delta]0Deg})*{-Cos[\[Delta]0Deg*DegToRad],1};
# {{\[CapitalDelta]x1,\[CapitalDelta]x2},{\[Alpha]Deg,\[Delta]Deg}}
# ];
#
#
# (* ::Text:: *)
# (*fAnnuli picks point in spherical annuli. The data are in "PixelCoordTab", "x1Ind" and "x2Ind" are the rows of the abscissa and ordinata, respectively, in PixelCoordTab, "CenterCoord" (format {Subscript[\[Theta], 1],Subscript[\[Theta], 2]}) is the position of the centre of the spherical annuli, "\[CapitalDelta]\[Theta]iTab" is a list giving the limiting radius of the annuli, "nPixMin" is the minimum number of points per annulus.*)
# (**)
# (*The function returns: "nAnnuli": number of annuli; "\[Theta]AnnuliTab" list of the radius of the centre of the annulus (geometric mean) with dispersion, "IndPixelWithinAnnuliTab", position in "PixelCoordTab" of the points per annulus; "nPixPerAnnulusTab": list of # of points per annulus.*)
#
#
# fAnnuli[PixelCoordTab_,x1Ind_,x2Ind_,CenterCoord_,\[CapitalDelta]\[Theta]iTab_(*annuli external and internal radii*),nPixMin_]:=
# Module[{IndWithPixelCoordTab,\[Theta]AnnuliTab,IndPixelWithinAnnuliTab={},IndPixelWithinAnnuliTabTmp1,IndPixelWithinAnnuliTabTmp2,\[Theta]PixPerAnnulusTab,nPixPerAnnulusTabTmp1,Ind,\[CapitalDelta]Ind,nAnnuli,nPixPerAnnulusTab},
# IndPixelWithinAnnuliTab={};
#
# IndWithPixelCoordTab=Table[{i,PixelCoordTab[[i,{x1Ind,x2Ind}]]},{i,Length[PixelCoordTab]}];
#
# IndPixelWithinAnnuliTabTmp2=Table[Select[IndWithPixelCoordTab,(\[CapitalDelta]\[Theta]iTab[[i]]^2<(#[[2,1]]-CenterCoord[[1]])^2+(#[[2,2]]-CenterCoord[[2]])^2<=\[CapitalDelta]\[Theta]iTab[[i+1]]^2)&][[All,1]],{i,2,Length[\[CapitalDelta]\[Theta]iTab]-1}];
#
# IndPixelWithinAnnuliTabTmp1=Join[{Select[IndWithPixelCoordTab,\[CapitalDelta]\[Theta]iTab[[1]]^2<=(#[[2,1]]-CenterCoord[[1]])^2+(#[[2,2]]-CenterCoord[[2]])^2<=\[CapitalDelta]\[Theta]iTab[[2]]^2&][[All,1]]},IndPixelWithinAnnuliTabTmp2];
#
# nAnnuli=Length[IndPixelWithinAnnuliTabTmp1];
# nPixPerAnnulusTab=Table[Length[IndPixelWithinAnnuliTabTmp1[[i]]],{i,nAnnuli}];
#
# Ind=1;
# While[
# Ind<nAnnuli,\[CapitalDelta]Ind=0;If[
# nPixPerAnnulusTab[[Ind]]<nPixMin,
# While[
# Ind+\[CapitalDelta]Ind<nAnnuli &&Total[nPixPerAnnulusTab[[Ind;;Ind+\[CapitalDelta]Ind]]]<nPixMin,
# \[CapitalDelta]Ind++
# ]
# ];
# AppendTo[IndPixelWithinAnnuliTab,Sort[IndPixelWithinAnnuliTabTmp1[[Ind;;Ind+\[CapitalDelta]Ind]]//Flatten]];
# Ind+=(\[CapitalDelta]Ind+1)
# ];
#
# If[
# Length[IndPixelWithinAnnuliTab[[-1]]]<nPixMin,
# IndPixelWithinAnnuliTab[[-2]]=IndPixelWithinAnnuliTab[[-2;;-1]]//Flatten;
# IndPixelWithinAnnuliTab=IndPixelWithinAnnuliTab[[1;;-2]]
# ];
#
# IndPixelWithinAnnuliTab=Select[IndPixelWithinAnnuliTab,#!={}&];
#
# nAnnuli=Length[IndPixelWithinAnnuliTab];
# nPixPerAnnulusTab=Table[Length[IndPixelWithinAnnuliTab[[i]]],{i,nAnnuli}];
# \[Theta]PixPerAnnulusTab=Table[Table[\[Sqrt](PixelCoordTab[[IndPixelWithinAnnuliTab[[i,j]],x1Ind]]^2+PixelCoordTab[[IndPixelWithinAnnuliTab[[i,j]],x2Ind]]^2),{j,Length[IndPixelWithinAnnuliTab[[i]]]}],{i,nAnnuli}];
# \[Theta]AnnuliTab=Table[{Mean[\[Theta]PixPerAnnulusTab[[i]]],StandardDeviation[\[Theta]PixPerAnnulusTab[[i]]]}//N,{i,nAnnuli}];
# (*
# \[Theta]AnnuliTab=Table[{GeometricMean[Select[\[Theta]PixPerAnnulusTab[[i]],#!=0.&]],StandardDeviation[\[Theta]PixPerAnnulusTab[[i]]]}//N,{i,nAnnuli}];
# *)
# {nAnnuli,\[Theta]AnnuliTab,IndPixelWithinAnnuliTab,nPixPerAnnulusTab}
# ];
#
#
# (*
#
# fAnnuliTmp1[PixelCoordTab_,x1Ind_,x2Ind_,CenterCoord_,\[CapitalDelta]\[Theta]iTab_(*annuli external and internal radii*),nPixMin_]:=
# Module[{IndWithPixelCoordTab,\[Theta]AnnuliTab,IndPixelWithinAnnuliTab={},IndPixelWithinAnnuliTabTmp1,IndPixelWithinAnnuliTabTmp2,\[Theta]PixPerAnnulusTab,nPixPerAnnulusTabTmp1,Ind,\[CapitalDelta]Ind,nAnnuli,nPixPerAnnulusTab},
# IndPixelWithinAnnuliTab={};
#
# IndWithPixelCoordTab=Table[{i,PixelCoordTab[[i,{x1Ind,x2Ind}]]},{i,Length[PixelCoordTab]}];
#
# \[Theta]PixInAnnuliTab=Partition[Sort[Table[{i,(PixelCoordTab[[i,x1Ind]]^2.+PixelCoordTab[[i,x2Ind]]^2.)^0.5},{i,Length[PixelCoordTab]}],#1[[1]]<#2[[1]]&],nPixMin];
#
# \[Theta]AnnuliTab=Table[{GeometricMean[\[Theta]PixPerAnnulusTab[[i]]],StandardDeviation[\[Theta]PixPerAnnulusTab[[i]]]}//N,{i,nAnnuli}];
#
# {nAnnuli,\[Theta]AnnuliTab,IndPixelWithinAnnuliTab,nPixPerAnnulusTab}
# ];
#
#
# IndPixelWithinAnnuliTabTmp2=Table[Select[IndWithPixelCoordTab,(\[CapitalDelta]\[Theta]iTab[[i]]^2<(#[[2,1]]-CenterCoord[[1]])^2+(#[[2,2]]-CenterCoord[[2]])^2<=\[CapitalDelta]\[Theta]iTab[[i+1]]^2)&][[All,1]],{i,2,Length[\[CapitalDelta]\[Theta]iTab]-1}];
#
# IndPixelWithinAnnuliTabTmp1=Join[{Select[IndWithPixelCoordTab,\[CapitalDelta]\[Theta]iTab[[1]]^2<=(#[[2,1]]-CenterCoord[[1]])^2+(#[[2,2]]-CenterCoord[[2]])^2<=\[CapitalDelta]\[Theta]iTab[[2]]^2&][[All,1]]},IndPixelWithinAnnuliTabTmp2];
#
# nAnnuli=Length[IndPixelWithinAnnuliTabTmp1];
# nPixPerAnnulusTab=Table[Length[IndPixelWithinAnnuliTabTmp1[[i]]],{i,nAnnuli}];
#
# Ind=1;
# While[
# Ind<nAnnuli,\[CapitalDelta]Ind=0;If[
# nPixPerAnnulusTab[[Ind]]<nPixMin,
# While[
# Ind+\[CapitalDelta]Ind<nAnnuli &&Total[nPixPerAnnulusTab[[Ind;;Ind+\[CapitalDelta]Ind]]]<nPixMin,
# \[CapitalDelta]Ind++
# ]
# ];
# AppendTo[IndPixelWithinAnnuliTab,Sort[IndPixelWithinAnnuliTabTmp1[[Ind;;Ind+\[CapitalDelta]Ind]]//Flatten]];
# Ind+=(\[CapitalDelta]Ind+1)
# ];
#
# If[
# Length[IndPixelWithinAnnuliTab[[-1]]]<nPixMin,
# IndPixelWithinAnnuliTab[[-2]]=IndPixelWithinAnnuliTab[[-2;;-1]]//Flatten;
# IndPixelWithinAnnuliTab=IndPixelWithinAnnuliTab[[1;;-2]]
# ];
#
# IndPixelWithinAnnuliTab=Select[IndPixelWithinAnnuliTab,#!={}&];
#
# nAnnuli=Length[IndPixelWithinAnnuliTab];
# nPixPerAnnulusTab=Table[Length[IndPixelWithinAnnuliTab[[i]]],{i,nAnnuli}];
# \[Theta]PixPerAnnulusTab=Table[Table[\[Sqrt](PixelCoordTab[[IndPixelWithinAnnuliTab[[i,j]],x1Ind]]^2+PixelCoordTab[[IndPixelWithinAnnuliTab[[i,j]],x2Ind]]^2),{j,Length[IndPixelWithinAnnuliTab[[i]]]}],{i,nAnnuli}];
# \[Theta]AnnuliTab=Table[{GeometricMean[\[Theta]PixPerAnnulusTab[[i]]],StandardDeviation[\[Theta]PixPerAnnulusTab[[i]]]}//N,{i,nAnnuli}];
#
# {nAnnuli,\[Theta]AnnuliTab,IndPixelWithinAnnuliTab,nPixPerAnnulusTab}
# ];
# *)
#
#
# (* ::Section:: *)
# (*Ellipticity*)
#
#
# (* ::Subsection:: *)
# (*Momenta of inertia*)
#
#
# \[Mu][m_,n_,DataTab_]:=Block[{DataTot,\[Mu]10,\[Mu]01,\[Mu]mn},
# DataTot=Total[DataTab[[All,3]]];
# \[Mu]10=Total[DataTab[[All,3]]*DataTab[[All,1]]]/DataTot;
# \[Mu]01=Total[DataTab[[All,3]]*DataTab[[All,2]]]/DataTot;
# \[Mu]mn=Total[DataTab[[All,3]]*(DataTab[[All,1]]-\[Mu]10)^m *(DataTab[[All,2]]-\[Mu]01)^n]/DataTot;
# {\[Mu]mn,\[Mu]10,\[Mu]01}
# ];
#
#
#
#
#
# fEllip[DataTab_,\[Theta]CentrCoord_]:=Block[{DataTot,\[Theta]1Centr,\[Theta]2Centr,Q11,Q12,Q22,QTab,\[Epsilon]1,eP1,\[Epsilon]2,eP2,\[Theta],\[Theta]Tmp1,eCompl,eComplMod,Eigensystemv},
# DataTot=Total[DataTab[[All,3]]];
# {\[Theta]1Centr,\[Theta]2Centr}=If[\[Theta]CentrCoord=={},
#  Total[DataTab[[All,3]]*DataTab[[All,{1,2}]]]/DataTot,
# \[Theta]CentrCoord];
# Q11=Total[DataTab[[All,3]]*(DataTab[[All,1]]-\[Theta]1Centr)^2]/DataTot;
# Q22=Total[DataTab[[All,3]]*(DataTab[[All,2]]-\[Theta]2Centr)^2]/DataTot;
# Q12=Total[DataTab[[All,3]]*(DataTab[[All,1]]-\[Theta]1Centr)*(DataTab[[All,2]]-\[Theta]2Centr)]/DataTot;
# (*eCompl={(Q11-Q22)/(Q11+Q22),(2.Q12)/(Q11+Q22)};
# eComplMod=Sqrt[eCompl.eCompl];*)
# QTab={{Q11,Q12},{Q12,Q22}};
# Eigensystemv=Eigensystem[QTab];
#
# eComplMod=Sqrt[(4.*Q12^2+(Q11-Q22)^2)/(Q11+Q22)^2];
# \[Epsilon]1=1.-Sqrt[(1.-eComplMod)/(1.+eComplMod)];
# \[Theta]={1.,180./\[Pi]}*ArcTan[Eigensystemv[[2,2,2]]/Eigensystemv[[2,2,1]]];
# (*
# eP2=(Eigensystemv[[1,1]]/Eigensystemv[[1,2]])^(1/2);
# \[Epsilon]2=1-(1/eP2);
# *)
#
# (*
# eP=(Sqrt[(1-Sqrt[eCompl.eCompl])/(1+Sqrt[1+eCompl..eCompl])])^-1;
#
# \[Theta]={1,RadToDeg} 1/2 ArcTan[eCompl[[2]]/eCompl[[1]]]/.{Q11->\[Mu]20,Q22->\[Mu]02,Q12->\[Mu]11};(*North through East*)
# *)
# (*\[Theta]={1,RadToDeg}(ArcTan[\[Mu]11/(EigensystemTmp1[[1,1]]^1-\[Mu]02)]);(*North through East*)*)
#
# Return[{\[Epsilon]1(*{\[Epsilon]1,\[Epsilon]2}*),\[Theta],{\[Theta]1Centr,\[Theta]2Centr},QTab}]
# ];
#
# fEllipticity[DataTab_,\[Theta]CentrCoord_]:=Block[{DataTot,\[Theta]1Centr,\[Theta]2Centr,Q11,Q12,Q22,QTab,\[Epsilon]1,eP1,\[Epsilon]2,eP2,\[Theta],\[Theta]Tmp1,eCompl,eComplMod,Eigensystemv},
# DataTot=Total[DataTab[[All,3]]];
# {\[Theta]1Centr,\[Theta]2Centr}=If[\[Theta]CentrCoord=={},
#  Total[DataTab[[All,3]]*DataTab[[All,{1,2}]]]/DataTot,
# \[Theta]CentrCoord];
# Q11=Total[DataTab[[All,3]]*(DataTab[[All,1]]-\[Theta]1Centr)^2]/DataTot;
# Q22=Total[DataTab[[All,3]]*(DataTab[[All,2]]-\[Theta]2Centr)^2]/DataTot;
# Q12=Total[DataTab[[All,3]]*(DataTab[[All,1]]-\[Theta]1Centr)*(DataTab[[All,2]]-\[Theta]2Centr)]/DataTot;
# (*eCompl={(Q11-Q22)/(Q11+Q22),(2.Q12)/(Q11+Q22)};
# eComplMod=Sqrt[eCompl.eCompl];*)
# QTab={{Q11,Q12},{Q12,Q22}};
# Eigensystemv=Eigensystem[QTab];
#
# eComplMod=Sqrt[(4.*Q12^2+(Q11-Q22)^2)/(Q11+Q22)^2];
# \[Epsilon]1=1.-Sqrt[(1.-eComplMod)/(1.+eComplMod)];
# \[Theta]=180.*ArcTan[Eigensystemv[[2,2,2]]/Eigensystemv[[2,2,1]]]/\[Pi];
# (*
# eP2=(Eigensystemv[[1,1]]/Eigensystemv[[1,2]])^(1/2);
# \[Epsilon]2=1-(1/eP2);
#
# qPerp=Sqrt[(Q11+Q22-Sqrt[Q11^2+4 Q12^2-2 Q11 Q22+Q22^2])/(Q11+Q22+Sqrt[Q11^2+4 Q12^2-2 Q11 Q22+Q22^2])];
# (*Eigenvaluesv=Eigenvalues[QTab];*)
# (*qPerp=Sqrt[Eigenvaluesv[[2]]/Eigenvaluesv[[1]]];*)
#
# *)
#
# (*
# eP=(Sqrt[(1-Sqrt[eCompl.eCompl])/(1+Sqrt[1+eCompl..eCompl])])^-1;
#
# \[Theta]={1,RadToDeg} 1/2 ArcTan[eCompl[[2]]/eCompl[[1]]]/.{Q11->\[Mu]20,Q22->\[Mu]02,Q12->\[Mu]11};(*North through East*)
# *)
# (*\[Theta]={1,RadToDeg}(ArcTan[\[Mu]11/(EigensystemTmp1[[1,1]]^1-\[Mu]02)]);(*North through East*)*)
#
# Return[{\[Epsilon]1(*{\[Epsilon]1,\[Epsilon]2}*),\[Theta],{\[Theta]1Centr,\[Theta]2Centr},QTab}]
# ];
#
#
# (* ::Subsection:: *)
# (*Random extraction*)
#
#
# fRandom[f_,p1Min_,p1Max_,nBoot_]:=Block[
# {p1Tmp1,fMax,iTmp1,iTmp2,p1,p1Tab},
# fMax=Max[f[p1Min],f[p1Max],NMaximize[{f[p1],p1Min<=p1<=p1Max},{p1,p1Min,p1Max}][[1]]];
# p1Tab=Reap[For[
# iTmp1=0;iTmp2=1,
# iTmp1<nBoot,
# iTmp2++,
# p1Tmp1=RandomReal[{p1Min,p1Max}];
# If[fMax*RandomReal[]<f[p1Tmp1],Sow[p1Tmp1];iTmp1++]
# ]][[2,1]];
# {p1Tab,iTmp1/iTmp2//N}];
#
#
# (* ::Subsection:: *)
# (*2D binning*)
#
#
# fBinnedList2D[DataTab_,nSteps_,nExtraSteps_]:=Block[{x1DataRange,x1Step,x2DataRange,x2Step,CountsTab,MaxCount,CentersTab,CentersAndCountsTab},
# x1DataRange={Min[DataTab[[All,1]]],Max[DataTab[[All,1]]]};
# x1Step=(x1DataRange[[2]]-x1DataRange[[1]])/nSteps[[1]];
# x2DataRange={Min[DataTab[[All,2]]],Max[DataTab[[All,2]]]};
# x2Step=(x2DataRange[[2]]-x2DataRange[[1]])/nSteps[[2]];
# CountsTab=BinCounts[DataTab,{x1DataRange[[1]]-nExtraSteps[[1]]*x1Step,x1DataRange[[2]]+nExtraSteps[[1]]*x1Step,x1Step},{x2DataRange[[1]]-nExtraSteps[[2]]*x2Step,x2DataRange[[2]]+nExtraSteps[[2]]*x2Step,x2Step}];
# MaxCount=Max[CountsTab];CentersTab=Table[{i,j},{i,x1DataRange[[1]]-(nExtraSteps[[1]]-1/2)*x1Step,x1DataRange[[2]]+(nExtraSteps[[1]]-1/2)*x1Step,x1Step},{j,x2DataRange[[1]]-(nExtraSteps[[2]]-1/2)*x2Step,x2DataRange[[2]]+(nExtraSteps[[2]]-1/2)*x2Step,x2Step}];
# CentersAndCountsTab=Partition[Table[{CentersTab[[i,j]],CountsTab[[i,j]]/MaxCount}//Flatten,{i,nSteps[[1]]+2nExtraSteps[[1]]},{j,nSteps[[2]]+2nExtraSteps[[2]]}]//Flatten,3];
# CentersAndCountsTab
# ];
#
#
# (* ::Section:: *)
# (*Inferred kernel distribution*)
#
#
# (* ::Text:: *)
# (*References in Sereno, M., Constraints on a quintessence model from gravitational lensing statistics, Monthly Notices of the Royal Astronomical Society, Volume 356 (2005), pp. 937-943.*)
#
#
# (* ::Text:: *)
# (*How to make an interpolated distribution out of an ensamble of data "TabY"*)
#
#
# KGaus[x_]:=1/Sqrt[2\[Pi]]*Exp[-(x^2/2)];
# KGaus2[t_,\[Sigma]1_,\[Sigma]2_]:=1/(2\[Pi](\[Sigma]1^2+\[Sigma]2^2))^(1/2) Exp[-(1/2) t^2/(\[Sigma]1^2+\[Sigma]2^2)];
#
#
# (* ::Text:: *)
# (*Smoothness parameter*)
#
#
# hSmooth[TabY_]:=0.9*(Min[StandardDeviation[TabY],InterquartileRange[TabY]/1.34])*(Length[TabY]^-0.2);
# (*https://stats.stackexchange.com/questions/90656/kernel-bandwidth-scotts-vs-silvermans-rules*)
# hSilverman[TabY_,nDim_]:=0.9*(Min[StandardDeviation[TabY],InterquartileRange[TabY]/1.349])*(Length[TabY]^-(1./(4.+nDim)));
#
#
# (* ::Text:: *)
# (*Reference kernel and interpolated distribution in the range {\[Eta]Min,\[Eta]Max}*)
#
#
# KRefBound[\[Eta]_,\[Eta]Mean_,\[Sigma]_,\[Eta]Min_,\[Eta]Max_]:=1/\[Sigma] (KGaus[(\[Eta]-\[Eta]Mean)/\[Sigma]]+KGaus[(\[Eta]+\[Eta]Mean-2\[Eta]Min)/\[Sigma]]+KGaus[(2\[Eta]Max-\[Eta]-\[Eta]Mean)/\[Sigma]]);
# fRefBound[\[Eta]_,h_,TabY_,\[Eta]Min_,\[Eta]Max_]:=1/Length[TabY] Sum[KRefBound[\[Eta],TabY[[i]],h,\[Eta]Min,\[Eta]Max ],{i,1,Length[TabY]}];
#
#
# (*
# {\[Eta]Minv,\[Eta]Maxv}={0.2,1.5};
# NIntegrate[KRefBound[\[Eta],0.5,0.3,\[Eta]Minv,\[Eta]Maxv],{\[Eta],\[Eta]Minv,\[Eta]Maxv}]
# *)
#
#
# (* ::Text:: *)
# (*Reference kernel and interpolated distribution in the range {0, \[Infinity]}*)
#
#
# KRef0Inf[\[Eta]_,\[Eta]i_,h_]:=1/h (KGaus[(\[Eta]-\[Eta]i)/h]+KGaus[(\[Eta]+\[Eta]i)/h]);
# fRef0Inf[\[Eta]_,h_,TabY_]:=1/Length[TabY] Sum[KRef0Inf[\[Eta],TabY[[i]],h ],{i,1,Length[TabY]}];
#
#
# (* ::Text:: *)
# (*Reference kernel and interpolated distribution in the range {-\[Infinity], +\[Infinity]}*)
#
#
# KRef[\[Eta]_,\[Eta]i_,h_]:=1/h KGaus[(\[Eta]-\[Eta]i)/h];
# fRef[\[Eta]_,h_,TabY_]:=1/Length[TabY] Sum[KRef0Inf[\[Eta],TabY[[i]],h ],{i,1,Length[TabY]}];
#
#
# fka[x_,TabY_,\[Sigma]_]:=(
# Nv=Length[TabY];
# \[Alpha]v=1/2;
# fpTmp1[xTmp1_]:=1/Nv Sum[KRef0Inf[xTmp1,TabY[[i]],\[Sigma] ],{i,1,Nv}];
# gv=Product[fpTmp1[TabY[[i]] ],{i,1,Nv}]^(1/Nv);
# \[Lambda]TabTmp1=Table[(fpTmp1[TabY[[i]] ]/gv)^-\[Alpha]v,{i,1,Nv}];
# 1/Nv Sum[KRef0Inf[x,TabY[[i]],\[Lambda]TabTmp1[[i]]*\[Sigma] ],{i,1,Nv}]
# );
#
#
# Mfka[TabY_,\[Sigma]_,\[Alpha]_]:=(
# Nv=LengthSmooth[TabY];
# \[Alpha]v=1/2;
# fpTmp1[x_]:=1/Nv Sum[KRef0Inf[x,TabY[[i]],\[Sigma] ],{i,1,Nv}];
# gvTmp1=Product[fpTmp1[TabY[[i]] ],{i,1,Nv}]^(1/Nv);
# \[Lambda]TabTmp1=Table[(fpTmp1[TabY[[i]] ]/gvTmp1)^-\[Alpha],{i,1,Nv}];
# 1/Nv^2 Sum[KGaus2[TabY[[i]]-TabY[[j]],\[Lambda]TabTmp1[[i]]\[Sigma],\[Lambda]TabTmp1[[i]]\[Sigma]],{i,1,Nv},{j,1,Nv}]-2/(Nv(Nv-1)) Sum[Sum[ KRef[TabY[[j]],TabY[[i]],\[Lambda]TabTmp1[[j]]\[Sigma]],{j,1,Nv}]-KRef[TabY[[i]],TabY[[i]],\[Lambda]TabTmp1[[i]]\[Sigma]],{i,1,Nv}]
# );
#
#
# MfkaMin[TabY_,\[Alpha]_]:=(
# \[Sigma]Tmp1=4hSmooth[TabY];
# MfkaTmp1=Mfka[TabY,\[Sigma]Tmp1,\[Alpha]];
#
# \[Sigma]Tmp2=\[Sigma]Tmp1/2;
# MfkaTmp2=Mfka[TabY,\[Sigma]Tmp2,\[Alpha]];
#
# MfkaMinTab={{\[Sigma]Tmp1,MfkaTmp1},{\[Sigma]Tmp2,MfkaTmp2}};
#
# nStepv=1;
# While[
# MfkaTmp2<MfkaTmp1,MfkaTmp1=MfkaTmp2;\[Sigma]Tmp1=\[Sigma]Tmp2;\[Sigma]Tmp2=\[Sigma]Tmp1/2;MfkaTmp2=Mfka[TabY,\[Sigma]Tmp2,\[Alpha]];nStepv++;AppendTo[MfkaMinTab,{\[Sigma]Tmp2,MfkaTmp2}]];
#
# MfkaMinTabFine=Union[{{(3\[Sigma]Tmp1)/4,Mfka[TabY,(3\[Sigma]Tmp1)/4,\[Alpha]]},{\[Sigma]Tmp1,MfkaTmp1}},Table[{\[Sigma],Mfka[TabY,\[Sigma],\[Alpha]]},{\[Sigma],(5\[Sigma]Tmp1)/4,2\[Sigma]Tmp1-\[Sigma]Tmp1/4,\[Sigma]Tmp1/4}]];
# \[Sigma]FinTmp1=Select[MfkaMinTabFine,#[[2]]==Min[MfkaMinTabFine]&][[1,1]];
# {\[Sigma]FinTmp1,Sort[Union[MfkaMinTabFine,MfkaMinTab]]});
#
#
# fkazFinder[Data_,zCol_,\[Alpha]_]:=(
# NDatavTmp1=Length[Data];
# If[zCol!=0,zDataTabTmp1=Data[[All,zCol]],zDataTabTmp1=Data];
#
# \[Sigma]kavTmp1=MfkaMin[zDataTabTmp1,1/2][[1]] ;
#
# {zMaxvTmp1,zMinvTmp1}={Max[zDataTabTmp1]+10\[Sigma]kavTmp1,Min[zDataTabTmp1]-10\[Sigma]kavTmp1};
# fpTmp2[x_]:=1/NDatavTmp1 Sum[KRef0Inf[x,zDataTabTmp1[[i]],\[Sigma]kavTmp1],{i,1,NDatavTmp1}];
# gvtmp2=Product[fpTmp2[zDataTabTmp1[[i]] ],{i,1,NDatavTmp1}]^(1/NDatavTmp1);
# \[Lambda]TabTmp2=Table[(fpTmp2[zDataTabTmp1[[i]] ]/gvtmp2)^-\[Alpha],{i,1,NDatavTmp1}];
# fkazTmp1[z_]:=1/NDatavTmp1 Sum[KRef0Inf[z,zDataTabTmp1[[i]],\[Lambda]TabTmp2[[i]]*\[Sigma]kavTmp1 ],{i,1,NDatavTmp1}];
# DfkazTmp1[z_]:=D[fkazTmp1[x],x]/.x->z;
#
# fkazInterTmp1=Interpolation[Table[{z,fkazTmp1[z]},{z,zMinvTmp1,zMaxvTmp1+1\[Sigma]kavTmp1,\[Sigma]kavTmp1/20}]];
# DfkazInterTmp1=Interpolation[Table[{z,DfkazTmp1[z]},{z,zMinvTmp1,zMaxvTmp1+1\[Sigma]kavTmp1,\[Sigma]kavTmp1/20}]];
# {fkazInterTmp1,DfkazInterTmp1,\[Sigma]kavTmp1,zMinvTmp1,zMaxvTmp1});
#
#
# fMainClump[Data_,zCol_,fkaz_,Dfkaz_]:=(
# zDataTabTmp1=Data[[All,zCol]];
# fkazInter[z_]:=fkaz[z];
# DfkazInter[z_]:=Dfkaz[z];
#
# aPisv=Sum[(DfkazInter[zDataTabTmp1[[i]]]/fkazInter[zDataTabTmp1[[i]]])^2,{i,1,Length[Data]}]^-1;
# MembTabTmp1={};
# Do[
# zn1=zDataTabTmp1[[i]];
# zn2=zn1+aPisv DfkazInter[zn1]/fkazInter[zn1];
# While[Abs[zn2/zn1-1]>10^-8,zn1=zn2;zn2=zn1+aPisv DfkazInter[zn1]/fkazInter[zn1]];
# AppendTo[MembTabTmp1,Round[zn2*10^4]/10^4//N],
# {i,1,Length[Data]}];
#
# NClusvTmp1=Length[Union[MembTabTmp1]];
#
# DataClusTabTmp1=Array[DataClusTmp1,NClusvTmp1];
# Do[DataClusTmp1[j]={},{j,1,NClusvTmp1}]
# Do[
# Do[
# If[
# MembTabTmp1[[i]]==Union[MembTabTmp1][[j]],AppendTo[DataClusTmp1[j],i] ],
# {j,1,NClusvTmp1}
# ],
# {i,1,Length[Data]}
# ];
#
# jMainvTmp1=Position[ Table[Length[DataClusTmp1[j]],{j,1,NClusvTmp1}],Max[Table[Length[DataClusTmp1[j]],{j,1,NClusvTmp1}] ]][[1,1]];
#
# {DataTab[[DataClusTmp1[jMainvTmp1]]],{NClusvTmp1,Union[MembTabTmp1]},Table[DataClusTmp1[i],{i,1,NClusvTmp1}]}
# );
#
#
# End[];
#
#
# EndPackage[];
