

BeginPackage["Bob`ContentBrowser`"]
Begin["`Private`"]

Bob`ContentBrowser`ResourceObjectBrowser[args___]:=Catch[
    ResourceObject;
    resourceObjectBrowser[args]]

resourceObjectBrowser[prop:("Local"|"UpdateAvailable")]:=With[{allinfo = With[{ids = ResourceSystemClient`Private`$localResources},
   Module[{n = Length[ids]},
    Progress`EvaluateWithProgress[
     Select[Table[Quiet[ResourceObject[ids[[i]]][All]], {i, n}], 
      AssociationQ], <|"Text" -> 
       "Comparing local versions to latest available versions", 
      "Progress" -> (i - 1)/n,
      "ItemCurrent" -> i, "ItemTotal" -> n|>]
    ]
   ]},
    resourceObjectBrowser[allinfo,prop]
]

resourceObjectBrowser[info:{KeyValuePattern[{"UUID"->_}]..}, selector_:All]:=
    With[{bytype=KeySort@GroupBy[filterResourceBrowserInfo[info,selector],#["ResourceType"]/.{"DataResource"->"Data"}&]},
        With[{bytypeloc=GroupBy[#,getLocation]&/@bytype},
            tabView[Normal[
                innerTabView/@(
                   ( Map[(makeResourceBrowserGrid@*DeleteMissing),
                       (Map[makeResourceBrowserPanel,bytypeloc,{3}])
                       ,{2}])
                    )]]

        ]
    ]

filterResourceBrowserInfo[info_,All|"Local"]:=info
filterResourceBrowserInfo[info_,"UpdateAvailable"]:=Select[
    info,updateAvailableQ]

makeResourceBrowserPanel[info:KeyValuePattern[{"Name"->name_,"Version"->v_}]]:=
    makeResourceBrowserPanel[name, v,ResourceSystemClient`Private`getAvailableVersion[info], info["UUID"]]
makeResourceBrowserPanel[info:KeyValuePattern[{"Name"->name_}]]:=c2cRButton[name,info["UUID"]]

updateAvailableQ[info:KeyValuePattern[{"Version"->v_String}]]:=TrueQ[ResourceSystemClient`Private`getAvailableVersion[info]=!=v]
updateAvailableQ[_]:=False

makeResourceBrowserPanel[name_String, v_String,v_, id_]:={nameversiongrid[name,id,v],SpanFromLeft}
makeResourceBrowserPanel[name_String, v_String,u_String,id_]:={nameversiongrid[name,id,v],
    updateButton[id,u,v]
    }
makeResourceBrowserPanel[___]:=Missing[]

nameversiongrid[name_,id_,v_]:=Grid[{{c2cRButton[name,id],versionlabel[v]}}]

versionlabel[v_]:=Style["("<>v<>")",10]

c2cRButton[name_String,uuid_]:=ClickToCopy[Style[name,14,FontFamily->"Source Code Pro"],Defer[ResourceObject[uuid]]]

updateButton[id_,u_,v_]:=DynamicModule[{label="Update to "<>u,en=True},
    Button[Dynamic@label,label=ProgressIndicator[Appearance->"Necklace"];label=updateResource[id,v];en=False,Enabled->Dynamic[en]]
    ]

getLocation[KeyValuePattern[{"RepositoryLocation"->_URL,"ResourceLocations"->{___,_LocalObject,___}}]]:=
    "Published/Locally Cached"

getLocation[KeyValuePattern[{"ResourceLocations"->{___,_CloudObject,_LocalObject,___}}]]:=
    "CloudDeployd/Locally Cached"

getLocation[KeyValuePattern[{"ResourceLocations"->{___,_CloudObject,___}}]]:=
    "CloudDeployed"

getLocation[KeyValuePattern[{"ResourceLocations"->{___,_LocalObject,___}}]]:=
    "Local"


getLocation[___]:="Unknown"

resourceObjectBrowser[___]:=$Failed

$MaxPanels=50;

makeResourceBrowserGrid[panels_]:=DynamicModule[{rows=Take[panels,UpTo[$MaxPanels]],n=$MaxPanels},
    
    Dynamic[
        Replace[makeresourceBrowserGrid[
            Append[
                rows,
                If[Length[panels]>n,
                    {Button["Show More",n+=$MaxPanels;rows=Take[panels,UpTo[n]]],SpanFromLeft}
                    ,
                    Nothing
                ]
            ]
        ],Except[_Grid]:>"Expired, reevaluate",{0}]
    ]
    
]/;Length[panels]>$MaxPanels

makeResourceBrowserGrid[panels_]:=makeresourceBrowserGrid[panels]

makeresourceBrowserGrid[expr_]:=Grid[expr,Alignment->Left]


getLocalResourceIDs[]:=Progress`EvaluateWithProgress[
     ResourceSystemClient`Private`$localResources, <|"Text" -> 
       "Collecting Local Resource IDs"|>]

updateResource[uuid_,_]:=With[{update=ResourceUpdate[ResourceObject[uuid]]},
If[ResourceSystemClient`ResourceObjectQ[update],
    "Updated to version "<>update["Version"],
    "Update Failed"
]
]


tabView[{HoldPattern[Rule][_,val_]}]:=Panel[val,ImageSize->Automatic]
tabView[data_]:=TabView[data,ImageSize->Automatic]

innerTabView[data:KeyValuePattern[{_->val_}]]:=Panel[val,ImageSize->Full]/;Length[data]===1
innerTabView[data_]:=TabView[data,ImageSize->Full]


End[]
EndPackage[]