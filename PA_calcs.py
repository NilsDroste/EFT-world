
################################################################################
# Initialization
# based on jeffreyhanson: https://github.com/jeffreyhanson/global-protected-areas/
################################################################################

## load libraries
import arcpy, sys, os
from arcpy import env

## setting up directories
arcpy.env.workspace = r'Y:\\Home\\droste\\GIS\\EFTglob\\ArcGISoutput' # check gdb scratch http://resources.arcgis.com/en/help/main/10.1/index.html#//001w00000047000000
arcpy.env.overwriteOutput = True

## set parallel processing factor to .8 times n cores
arcpy.env.parallelProcessingFactor="80%"

## create intermediate gdb
arcpy.CreateFileGDB_management('Y:\\Home\\droste\\GIS\\EFTglob\\ArcGISoutput', 'intermediate_Poly.gdb')
arcpy.CreateFileGDB_management('Y:\\Home\\droste\\GIS\\EFTglob\\ArcGISoutput', 'intermediate_Point.gdb')

## copy data to intermediate gdb
arcpy.CopyFeatures_management ("Y:\\Home\\droste\\GIS\\EFTglob\\input\\WDPA_May2017_Public.gdb\\WDPA_poly_May2017", "Y:\\Home\\droste\\GIS\\EFTglob\\ArcGISoutput\\intermediate_Poly.gdb\\WDPA_poly_May2017")
arcpy.CopyFeatures_management ("Y:\\Home\\droste\\GIS\\EFTglob\\input\\WDPA_May2017_Public.gdb\\WDPA_point_May2017", "Y:\\Home\\droste\\GIS\\EFTglob\\ArcGISoutput\\intermediate_Point.gdb\\WDPA_point_May2017")


################################################################################
# Repair geometry issues
################################################################################

#TODO: divide into seperate scrips OR fork https://github.com/jeffreyhanson/global-protected-areas/

## load data from intermediate gdb
arcpy.MakeFeatureLayer_management ("Y:\\Home\\droste\\GIS\\EFTglob\\ArcGISoutput\\intermediate_Poly.gdb\\WDPA_poly_May2017", "WDPA_poly_May2017")
arcpy.MakeFeatureLayer_management ("Y:\\Home\\droste\\GIS\\EFTglob\\ArcGISoutput\\intermediate_Point.gdb\\WDPA_point_May2017", "WDPA_point_May2017")

## repair polygon geometry
for i in list(range(1,4)):
    arcpy.RepairGeometry_management("WDPA_poly_May2017")

## repair point geometry
for i in list(range(1,4)):
    arcpy.RepairGeometry_management("WDPA_point_May2017")


################################################################################
# omitting areas with given parameters
################################################################################

## load data from repaired intermediate gdb
arcpy.MakeFeatureLayer_management ("Y:\\Home\\droste\\GIS\\EFTglob\\ArcGISoutput\\intermediate_Poly.gdb\\WDPA_poly_May2017", "WDPA_poly_May2017")
arcpy.MakeFeatureLayer_management ("Y:\\Home\\droste\\GIS\\EFTglob\\ArcGISoutput\\intermediate_Point.gdb\\WDPA_point_May2017", "WDPA_point_May2017")

## parse fields in shapefiles
for file in ( "WDPA_poly_May2017", "WDPA_point_May2017" ):
    fields = [field.name for field in arcpy.ListFields(file)]
    for i in range(len(fields)):
        fields[i] = fields[i].encode('utf8')

## create intermediate subselection gdb
arcpy.CreateFileGDB_management('Y:\\Home\\droste\\GIS\\EFTglob\\ArcGISoutput', 'intermed_Poly_subsel.gdb')
arcpy.CreateFileGDB_management('Y:\\Home\\droste\\GIS\\EFTglob\\ArcGISoutput', 'intermed_Point_subsel.gdb')

## subselect actual PA
arcpy.Select_analysis("WDPA_poly_May2017", "Y:\\Home\\droste\\GIS\\EFTglob\\ArcGISoutput\\intermed_Poly_subsel.gdb\\WDPA_poly_May2017" , ' "STATUS" NOT IN (\'Not Reported\', \'Proposed\') AND NOT "MARINE" = \'2\' ')
arcpy.Select_analysis("WDPA_point_May2017", "Y:\\Home\\droste\\GIS\\EFTglob\\ArcGISoutput\\intermed_Point_subsel.gdb\\WDPA_point_May2017" , ' "STATUS" NOT IN (\'Not Reported\', \'Proposed\') AND NOT "MARINE" = \'2\' AND "REP_AREA" > 0 ' )


################################################################################
# Reproject point data
################################################################################

## load data from subset intermediate gdb
arcpy.MakeFeatureLayer_management ("Y:\\Home\\droste\\GIS\\EFTglob\\ArcGISoutput\\intermed_Point_subsel.gdb\\WDPA_point_May2017", "WDPA_point_May2017")

## projection parameters
output_coordinate_reference_system = 54017 # World_Behrman
distance_coordinate_reference_system = 54002 # World_Equidistant_Cylindrical

## create intermediate subselection gdb
arcpy.CreateFileGDB_management('Y:\\Home\\droste\\GIS\\EFTglob\\ArcGISoutput', 'intermed_Point_subsel_reproj.gdb')

## reprojecting point data to distance coordinate system
arcpy.Project_management("WDPA_point_May2017", "Y:\\Home\\droste\\GIS\\EFTglob\\ArcGISoutput\\intermed_Point_subsel_reproj.gdb\\WDPA_point_May2017", distance_coordinate_reference_system)


################################################################################
# Buffering points
################################################################################

## load data from subset intermediate gdb
arcpy.MakeFeatureLayer_management ("Y:\\Home\\droste\\GIS\\EFTglob\\ArcGISoutput\\intermed_Point_subsel_reproj.gdb\\WDPA_point_May2017", "WDPA_point_May2017")

## calculate radius
arcpy.AddField_management("WDPA_point_May2017", 'REP_RADIUS', 'DOUBLE')
arcpy.CalculateField_management("WDPA_point_May2017", 'REP_RADIUS', 'math.sqrt(!REP_AREA!/math.pi)', 'PYTHON_9.3')

## create intermediate buffered gdb
arcpy.CreateFileGDB_management('Y:\\Home\\droste\\GIS\\EFTglob\\ArcGISoutput', 'intermed_Point_buff.gdb')

## buffer points to polygon
arcpy.Buffer_analysis("WDPA_point_May2017", "Y:\\Home\\droste\\GIS\\EFTglob\\ArcGISoutput\\intermed_Point_buff.gdb\\WDPA_point_May2017", 'REP_RADIUS')


################################################################################
# reproject buffered point layer
################################################################################

## load data from subset intermediate gdb
arcpy.MakeFeatureLayer_management ("Y:\\Home\\droste\\GIS\\EFTglob\\ArcGISoutput\\intermed_Point_buff.gdb\\WDPA_point_May2017", "WDPA_point_May2017")

## projection parameters
output_coordinate_reference_system = 54017 # World_Behrman

## create intermediate buffered gdb
arcpy.CreateFileGDB_management('Y:\\Home\\droste\\GIS\\EFTglob\\ArcGISoutput', 'intermed_Point_buff_rprj.gdb')

## reprojecting point data to output coordinate system
arcpy.Project_management("WDPA_point_May2017", "Y:\\Home\\droste\\GIS\\EFTglob\\ArcGISoutput\\intermed_Point_buff_rprj.gdb\\WDPA_point_May2017", output_coordinate_reference_system)


################################################################################
# reprojecting polygon data to output coordinate system
################################################################################

## projection parameters
output_coordinate_reference_system = 54017 # World_Behrman

## load data from subset intermediate gdb
arcpy.MakeFeatureLayer_management ("Y:\\Home\\droste\\GIS\\EFTglob\\ArcGISoutput\\intermed_Poly_subsel.gdb\\WDPA_poly_May2017", "WDPA_poly_May2017")

## create intermediate reprojection gdb
arcpy.CreateFileGDB_management('Y:\\Home\\droste\\GIS\\EFTglob\\ArcGISoutput', 'intermed_Poly_reproj.gdb')

## reprojecting poly data to output coordinate system
arcpy.Project_management("WDPA_poly_May2017", "Y:\\Home\\droste\\GIS\\EFTglob\\ArcGISoutput\\intermed_Poly_reproj.gdb\\WDPA_poly_May2017", output_coordinate_reference_system)


################################################################################
# Simplify Polygon PA
################################################################################

## simplification parameters
algorithm = "POINT_REMOVE"
tolerance = "100 METERS"
minimum_area = "0 SQUAREMETERS"
error_option="NO_CHECK"
collapsed_point_option="NO_KEEP"

## load data from reprojected intermediate gdb
arcpy.MakeFeatureLayer_management ("Y:\\Home\\droste\\GIS\\EFTglob\\ArcGISoutput\\intermed_Poly_reproj.gdb\\WDPA_poly_May2017", "WDPA_poly_May2017")

## create intermediate reprojection gdb
arcpy.CreateFileGDB_management('Y:\\Home\\droste\\GIS\\EFTglob\\ArcGISoutput', 'intermed_Poly_rprj_smpl.gdb')

## processing simplification
arcpy.SimplifyPolygon_cartography(
    "WDPA_poly_May2017", "Y:\\Home\\droste\\GIS\\EFTglob\\ArcGISoutput\\intermed_Poly_rprj_smpl.gdb\\WDPA_poly_May2017",
    algorithm, tolerance, minimum_area,
    error_option, collapsed_point_option
)


################################################################################
# merging point and polygon data
################################################################################

## projection parameters
output_coordinate_reference_system = 54017 # World_Behrman

## load data from reprojected intermediate gdb
arcpy.MakeFeatureLayer_management ("Y:\\Home\\droste\\GIS\\EFTglob\\ArcGISoutput\\intermed_Poly_rprj_smpl.gdb\\WDPA_poly_May2017", "WDPA_poly_May2017")
arcpy.MakeFeatureLayer_management ("Y:\\Home\\droste\\GIS\\EFTglob\\ArcGISoutput\\intermed_Point_buff_rprj.gdb\\WDPA_point_May2017", "WDPA_point_May2017")

## checkout sa
arcpy.CheckOutExtension("Spatial")

## create intermediate reprojection gdb
arcpy.CreateFileGDB_management('Y:\\Home\\droste\\GIS\\EFTglob\\ArcGISoutput', 'allIUCN.gdb')

## merging
arcpy.Merge_management(["WDPA_poly_May2017", "WDPA_point_May2017"], "Y:\\Home\\droste\\GIS\\EFTglob\\ArcGISoutput\\allIUCN.gdb\\allIUCN", output_coordinate_reference_system)


################################################################################
# reprojecting country data to output coordinate system
################################################################################

## projection parameters
output_coordinate_reference_system = 54017 # World_Behrman

## load data from subset intermediate gdb
arcpy.MakeFeatureLayer_management ("Y:\\Home\\droste\\GIS\\EFTglob\\input\\gadm28_levels.gdb\\adm0", "countries")

## create intermediate reprojection gdb
arcpy.CreateFileGDB_management('Y:\\Home\\droste\\GIS\\EFTglob\\ArcGISoutput', 'intermed_countries_rprj.gdb')

## reprojecting poly data to output coordinate system
arcpy.Project_management("countries", "Y:\\Home\\droste\\GIS\\EFTglob\\ArcGISoutput\\intermed_countries_rprj.gdb\\intermed_countries_rprj", output_coordinate_reference_system)


################################################################################

# subselection for IUCN categories

################################################################################

import arcpy, sys, os
from arcpy import env

## setting up directories

arcpy.env.workspace = "Y:/Home/droste/GIS/EFTglob/ArcGISoutput"
arcpy.env.overwriteOutput = True

## set parallel processing factor to 1 times n cores
arcpy.env.parallelProcessingFactor="100%"

## load reproj country data
arcpy.MakeFeatureLayer_management(arcpy.env.workspace + "/" + "intermed_countries_rprj.gdb/intermed_countries_rprj", "countries")
print ("country data loaded at:"), datetime.datetime.now().time()

## repair and load IUCN PA data
arcpy.RepairGeometry_management(arcpy.env.workspace + "/allIUCN.gdb/allIUCN")
print ("repaired all IUCN PA layer")
arcpy.MakeFeatureLayer_management(arcpy.env.workspace + "/allIUCN.gdb/allIUCN", "allIUCN")
print ("PA data loaded at:"), datetime.datetime.now().time()

## create a scratch gdb
arcpy.env.scratchWorkspace = "C:/Users/droste/Documents/EFTglob" # arcpy.env.workspace

#loop over IUCN categories and tabulate intersection with countries without overlapping other IUCN PA (decreasing hierarchy Ia beats all others)
for cat in ['Ia', 'Ib', 'II', 'III', 'IV', 'V', 'VI']:
    arcpy.AddField_management("countries",  "PAcat%s"%(cat), 'DOUBLE')
    arcpy.Select_analysis("allIUCN", arcpy.env.scratchGDB + "/" + "Cat%s_to_diss"%(cat), '"IUCN_CAT"' "=" "'%s'"%(cat) )
    if cat == 'Ia':
        print ("selecting IUCN category %s at: "%(cat)), datetime.datetime.now().time()
        arcpy.Dissolve_management(arcpy.env.scratchGDB + "/" + "Cat%s_to_diss"%(cat), arcpy.env.scratchGDB + "/" + "Cat%s_to_intrsec"%(cat), "ISO3","", "MULTI_PART")
        print ("dissolved IUCN category %s at: "%(cat)), datetime.datetime.now().time()
        arcpy.RepairGeometry_management(arcpy.env.scratchGDB + "/" + "Cat%s_to_intrsec"%(cat))
        print ("repaired IUCN category %s at: "%(cat)), datetime.datetime.now().time()
        arcpy.Delete_management(arcpy.env.scratchGDB + "/" + "Cat%s_to_diss"%(cat))
    if cat == 'Ib':
        print ("selecting IUCN category %s at: "%(cat)), datetime.datetime.now().time()
        arcpy.Dissolve_management(arcpy.env.scratchGDB + "/" + "Cat%s_to_diss"%(cat),  arcpy.env.scratchGDB + "/" + "Cat%s_to_erase"%(cat), "ISO3","", "MULTI_PART")
        print ("dissolved IUCN category %s at: "%(cat)), datetime.datetime.now().time()
        arcpy.Delete_management(arcpy.env.scratchGDB + "/" + "Cat%s_to_diss"%(cat))
        arcpy.RepairGeometry_management(arcpy.env.scratchGDB + "/" + "Cat%s_to_erase"%(cat))
        print ("repaired IUCN category %s at: "%(cat)), datetime.datetime.now().time()
        arcpy.Erase_analysis(arcpy.env.scratchGDB + "/" + "Cat%s_to_erase"%(cat), arcpy.env.scratchGDB + "/" + "CatIa_to_intrsec", arcpy.env.scratchGDB + "/" + "Cat%s_to_intrsec"%(cat) )
        print ("erased IUCN category %s at: "%(cat)), datetime.datetime.now().time()
        arcpy.RepairGeometry_management(arcpy.env.scratchGDB + "/" + "Cat%s_to_intrsec"%(cat))
        print ("repaired IUCN category %s at: "%(cat)), datetime.datetime.now().time()
        arcpy.Delete_management(arcpy.env.scratchGDB + "/" + "CatIa_to_intrsec" )
    if cat == 'II':
        arcpy.Delete_management(arcpy.env.scratchGDB + "/" + "CatIb_to_intrsec" )
        print ("selecting IUCN category %s at: "%(cat)), datetime.datetime.now().time()
        arcpy.Dissolve_management(arcpy.env.scratchGDB + "/" + "Cat%s_to_diss"%(cat),  arcpy.env.scratchGDB + "/" + "Cat%s_to_erase"%(cat), "ISO3","", "MULTI_PART")
        print ("dissolved IUCN category %s at: "%(cat)), datetime.datetime.now().time()
        arcpy.Delete_management(arcpy.env.scratchGDB + "/" + "Cat%s_to_diss"%(cat))
        arcpy.RepairGeometry_management(arcpy.env.scratchGDB + "/" + "Cat%s_to_erase"%(cat))
        print ("repaired IUCN category %s at: "%(cat)), datetime.datetime.now().time()
        arcpy.Select_analysis("allIUCN", arcpy.env.scratchGDB + "/" + "CatIatoIb_to_diss", "IUCN_CAT = 'Ia' AND IUCN_CAT = 'Ib'")
        print ("selected IUCN category Ia to Ib at: "), datetime.datetime.now().time()
        arcpy.Dissolve_management(arcpy.env.scratchGDB + "/" + "CatIatoIb_to_diss",  arcpy.env.scratchGDB + "/" + "CatIatoIb_to_erase", "ISO3","", "MULTI_PART")
        print ("dissolved IUCN category Ia to Ib at: "), datetime.datetime.now().time()
        arcpy.Delete_management(arcpy.env.scratchGDB + "/" + "CatIatoIb_to_diss")
        arcpy.RepairGeometry_management(arcpy.env.scratchGDB + "/" + "CatIatoIb_to_erase")
        print ("repaired IUCN category Ia to Ib  at: "), datetime.datetime.now().time()
        arcpy.Erase_analysis(arcpy.env.scratchGDB + "/" + "Cat%s_to_erase"%(cat), arcpy.env.scratchGDB + "/" + "CatIatoIb_to_erase", arcpy.env.scratchGDB + "/" + "Cat%s_to_intrsec"%(cat) )
        print ("erased IUCN category %s at: "%(cat)), datetime.datetime.now().time()
        arcpy.Delete_management(arcpy.env.scratchGDB + "/" + "CatIatoIb_to_erase")
        arcpy.RepairGeometry_management(arcpy.env.scratchGDB + "/" + "Cat%s_to_intrsec"%(cat))
        print ("repaired IUCN category %s at: "%(cat)), datetime.datetime.now().time()
    if cat == 'III':
        arcpy.Delete_management(arcpy.env.scratchGDB + "/" + "CatII_to_intrsec" )
        print ("selecting IUCN category %s at: "%(cat)), datetime.datetime.now().time()
        arcpy.Dissolve_management(arcpy.env.scratchGDB + "/" + "Cat%s_to_diss"%(cat),  arcpy.env.scratchGDB + "/" + "Cat%s_to_erase"%(cat), "ISO3","", "MULTI_PART")
        print ("dissolved IUCN category %s at: "%(cat)), datetime.datetime.now().time()
        arcpy.Delete_management(arcpy.env.scratchGDB + "/" + "Cat%s_to_diss"%(cat))
        arcpy.RepairGeometry_management(arcpy.env.scratchGDB + "/" + "Cat%s_to_erase"%(cat))
        print ("repaired IUCN category %s at: "%(cat)), datetime.datetime.now().time()
        arcpy.Select_analysis("allIUCN", arcpy.env.scratchGDB + "/" + "CatIatoII_to_diss", "IUCN_CAT = 'Ia' AND IUCN_CAT = 'Ib' AND IUCN_CAT = 'II'")
        print ("selected IUCN category Ia to II at: "), datetime.datetime.now().time()
        arcpy.Dissolve_management(arcpy.env.scratchGDB + "/" + "CatIatoII_to_diss",  arcpy.env.scratchGDB + "/" + "CatIatoII_to_erase", "ISO3","", "MULTI_PART")
        print ("dissolved IUCN category Ia to II at: "), datetime.datetime.now().time()
        arcpy.Delete_management(arcpy.env.scratchGDB + "/" + "CatIatoII_to_diss")
        arcpy.RepairGeometry_management(arcpy.env.scratchGDB + "/" + "CatIatoII_to_erase")
        print ("repaired IUCN category Ia to II  at: "), datetime.datetime.now().time()
        arcpy.Erase_analysis(arcpy.env.scratchGDB + "/" + "Cat%s_to_erase"%(cat), arcpy.env.scratchGDB + "/" + "CatIatoII_to_erase", arcpy.env.scratchGDB + "/" + "Cat%s_to_intrsec"%(cat) )
        print ("erased IUCN category %s at: "%(cat)), datetime.datetime.now().time()
        arcpy.Delete_management(arcpy.env.scratchGDB + "/" + "CatIatoII_to_erase")
        arcpy.RepairGeometry_management(arcpy.env.scratchGDB + "/" + "Cat%s_to_intrsec"%(cat))
        print ("repaired IUCN category %s at: "%(cat)), datetime.datetime.now().time()
    if cat == 'IV':
        arcpy.Delete_management(arcpy.env.scratchGDB + "/" + "CatIII_to_intrsec" )
        print ("selecting IUCN category %s at: "%(cat)), datetime.datetime.now().time()
        arcpy.Dissolve_management(arcpy.env.scratchGDB + "/" + "Cat%s_to_diss"%(cat),  arcpy.env.scratchGDB + "/" + "Cat%s_to_erase"%(cat), "ISO3","", "MULTI_PART")
        print ("dissolved IUCN category %s at: "%(cat)), datetime.datetime.now().time()
        arcpy.Delete_management(arcpy.env.scratchGDB + "/" + "Cat%s_to_diss"%(cat))
        arcpy.RepairGeometry_management(arcpy.env.scratchGDB + "/" + "Cat%s_to_erase"%(cat))
        print ("repaired IUCN category %s at: "%(cat)), datetime.datetime.now().time()
        arcpy.Select_analysis("allIUCN", arcpy.env.scratchGDB + "/" + "CatIatoIII_to_diss", "IUCN_CAT = 'Ia' AND IUCN_CAT = 'Ib' AND IUCN_CAT = 'II' AND IUCN_CAT = 'III'")
        print ("selected IUCN category Ia to III at: "), datetime.datetime.now().time()
        arcpy.Dissolve_management(arcpy.env.scratchGDB + "/" + "CatIatoIII_to_diss",  arcpy.env.scratchGDB + "/" + "CatIatoIII_to_erase", "ISO3","", "MULTI_PART")
        print ("dissolved IUCN category Ia to III at: "), datetime.datetime.now().time()
        arcpy.Delete_management(arcpy.env.scratchGDB + "/" + "CatIatoIII_to_diss")
        arcpy.RepairGeometry_management(arcpy.env.scratchGDB + "/" + "CatIatoIII_to_erase")
        print ("repaired IUCN category Ia to III  at: "), datetime.datetime.now().time()
        arcpy.Erase_analysis(arcpy.env.scratchGDB + "/" + "Cat%s_to_erase"%(cat), arcpy.env.scratchGDB + "/" + "CatIatoIII_to_erase", arcpy.env.scratchGDB + "/" + "Cat%s_to_intrsec"%(cat) )
        print ("erased IUCN category %s at: "%(cat)), datetime.datetime.now().time()
        arcpy.Delete_management(arcpy.env.scratchGDB + "/" + "CatIatoIII_to_erase")
        arcpy.RepairGeometry_management(arcpy.env.scratchGDB + "/" + "Cat%s_to_intrsec"%(cat))
        print ("repaired IUCN category %s at: "%(cat)), datetime.datetime.now().time()
    if cat == 'V':
        arcpy.Delete_management(arcpy.env.scratchGDB + "/" + "CatIV_to_intrsec" )
        print ("selecting IUCN category %s at: "%(cat)), datetime.datetime.now().time()
        arcpy.Dissolve_management(arcpy.env.scratchGDB + "/" + "Cat%s_to_diss"%(cat),  arcpy.env.scratchGDB + "/" + "Cat%s_to_erase"%(cat), "ISO3","", "MULTI_PART")
        print ("dissolved IUCN category %s at: "%(cat)), datetime.datetime.now().time()
        arcpy.Delete_management(arcpy.env.scratchGDB + "/" + "Cat%s_to_diss"%(cat))
        arcpy.RepairGeometry_management(arcpy.env.scratchGDB + "/" + "Cat%s_to_erase"%(cat))
        print ("repaired IUCN category %s at: "%(cat)), datetime.datetime.now().time()
        arcpy.Select_analysis("allIUCN", arcpy.env.scratchGDB + "/" + "CatIatoIV_to_diss", "IUCN_CAT = 'Ia' AND IUCN_CAT = 'Ib' AND IUCN_CAT = 'II' AND IUCN_CAT = 'III' AND IUCN_CAT = 'IV'")
        print ("selected IUCN category Ia to IV at: "), datetime.datetime.now().time()
        arcpy.Dissolve_management(arcpy.env.scratchGDB + "/" + "CatIatoIV_to_diss",  arcpy.env.scratchGDB + "/" + "CatIatoIV_to_erase", "ISO3","", "MULTI_PART")
        print ("dissolved IUCN category Ia to IV at: "), datetime.datetime.now().time()
        arcpy.Delete_management(arcpy.env.scratchGDB + "/" + "CatIatoIV_to_diss")
        arcpy.RepairGeometry_management(arcpy.env.scratchGDB + "/" + "CatIatoIV_to_erase")
        print ("repaired IUCN category Ia to IV  at: "), datetime.datetime.now().time()
        arcpy.Erase_analysis(arcpy.env.scratchGDB + "/" + "Cat%s_to_erase"%(cat), arcpy.env.scratchGDB + "/" + "CatIatoIV_to_erase", arcpy.env.scratchGDB + "/" + "Cat%s_to_intrsec"%(cat) )
        print ("erased IUCN category %s at: "%(cat)), datetime.datetime.now().time()
        arcpy.Delete_management(arcpy.env.scratchGDB + "/" + "CatIatoIV_to_erase")
        arcpy.RepairGeometry_management(arcpy.env.scratchGDB + "/" + "Cat%s_to_intrsec"%(cat))
        print ("repaired IUCN category %s at: "%(cat)), datetime.datetime.now().time()
    if cat == 'VI':
        arcpy.Delete_management(arcpy.env.scratchGDB + "/" + "CatV_to_intrsec" )
        print ("selecting IUCN category %s at: "%(cat)), datetime.datetime.now().time()
        arcpy.Dissolve_management(arcpy.env.scratchGDB + "/" + "Cat%s_to_diss"%(cat),  arcpy.env.scratchGDB + "/" + "Cat%s_to_erase"%(cat), "ISO3","", "MULTI_PART")
        print ("dissolved IUCN category %s at: "%(cat)), datetime.datetime.now().time()
        arcpy.Delete_management(arcpy.env.scratchGDB + "/" + "Cat%s_to_diss"%(cat))
        arcpy.RepairGeometry_management(arcpy.env.scratchGDB + "/" + "Cat%s_to_erase"%(cat))
        print ("repaired IUCN category %s at: "%(cat)), datetime.datetime.now().time()
        arcpy.Select_analysis("allIUCN", arcpy.env.scratchGDB + "/" + "CatIatoV_to_diss", "IUCN_CAT = 'Ia' AND IUCN_CAT = 'Ib' AND IUCN_CAT = 'II' AND IUCN_CAT = 'III' AND IUCN_CAT = 'IV' AND IUCN_CAT = 'V'")
        print ("selected IUCN category Ia to V at: "), datetime.datetime.now().time()
        arcpy.Dissolve_management(arcpy.env.scratchGDB + "/" + "CatIatoV_to_diss",  arcpy.env.scratchGDB + "/" + "CatIatoV_to_erase", "ISO3","", "MULTI_PART")
        print ("dissolved IUCN category Ia to V at: "), datetime.datetime.now().time()
        arcpy.Delete_management(arcpy.env.scratchGDB + "/" + "CatIatoV_to_diss")
        arcpy.RepairGeometry_management(arcpy.env.scratchGDB + "/" + "CatIatoV_to_erase")
        print ("repaired IUCN category Ia to V  at: "), datetime.datetime.now().time()
        arcpy.Erase_analysis(arcpy.env.scratchGDB + "/" + "Cat%s_to_erase"%(cat), arcpy.env.scratchGDB + "/" + "CatIatoV_to_erase", arcpy.env.scratchGDB + "/" + "Cat%s_to_intrsec"%(cat) )
        print ("erased IUCN category %s at: "%(cat)), datetime.datetime.now().time()
        arcpy.Delete_management(arcpy.env.scratchGDB + "/" + "CatIatoV_to_erase")
        arcpy.RepairGeometry_management(arcpy.env.scratchGDB + "/" + "Cat%s_to_intrsec"%(cat))
        print ("repaired IUCN category %s at: "%(cat)), datetime.datetime.now().time()
    arcpy.Delete_management(arcpy.env.scratchGDB + "/" + "Cat%s_to_erase"%(cat))
    arcpy.TabulateIntersection_analysis("countries", "ISO", arcpy.env.scratchGDB + "/" + "Cat%s_to_intrsec"%(cat), arcpy.env.scratchGDB + "/" + "y_tab_%s"%(cat))
    print ("tabulated %s PA intersection with countries at: "%(cat)), datetime.datetime.now().time()
    with arcpy.da.UpdateCursor("countries",["ISO", "Shape_Area", "PAcat%s"%(cat)]) as upd_cur:
        for upd_row in upd_cur:
            with arcpy.da.SearchCursor(arcpy.env.scratchGDB + "/" + "y_tab_%s"%(cat),["ISO", "PERCENTAGE"]) as search_cur:
                for search_row in search_cur:
                    if search_row[0] == upd_row[0]:
                        upd_row[2] = search_row[1]
                        upd_cur.updateRow(upd_row)
                if not upd_row[2]:
                    upd_row[2] = 0
                    upd_cur.updateRow(upd_row)
    arcpy.Delete_management(arcpy.env.scratchGDB + "/" + "y_tab_%s"%(cat))

arcpy.Delete_management(arcpy.env.scratchGDB + "/" + "CatVI_to_intrsec" )

## create final gdb
arcpy.CreateFileGDB_management('Y:/Home/droste/GIS/EFT-glob/ArcGISoutput', 'countries.gdb')

# copy files there
arcpy.CopyFeatures_management("countries", "Y:/Home/droste/GIS/EFT-glob/ArcGISoutput/countries.gdb/countries")


################################################################################
# write gdb table as csv file
################################################################################

import csv

### NOTE: setdefaultencoding HACK to solve ascii codec issues /w writing to
import sys
reload(sys)
sys.setdefaultencoding("utf-8")
### NOTE: this is generally discouraged: http://stackoverflow.com/questions/28657010/dangers-of-sys-setdefaultencodingutf-8

## load reproj country data
arcpy.MakeFeatureLayer_management("Y:\\Home\\droste\\GIS\\EFTglob\\ArcGISoutput\\countries.gdb\\countries", "countries")

## get list of field names for countries gdb table
fields = [field.name for field in arcpy.ListFields("countries")]

## use a SearchCursor to write gdb rows to csv output file
with open("Y:\\Home\\droste\\GIS\\EFTglob\\ArcGISoutput\\countries.csv",'wb') as f:
    w = csv.writer(f)
    w.writerow(fields)
    for row in arcpy.da.SearchCursor("countries", fields):
        w.writerow(row[:])
    del(row)

### free up memory remove data
mxd = arcpy.mapping.MapDocument("CURRENT")
for df in arcpy.mapping.ListDataFrames(mxd):
    for lyr in arcpy.mapping.ListLayers(mxd, "", df):
        if lyr.name == "countries":
            arcpy.mapping.RemoveLayer(df, lyr)
        if lyr.name == "allIUCN":
            arcpy.mapping.RemoveLayer(df, lyr)

### clear workspace of projections
sr = arcpy.SpatialReference()
sr.loadFromString('{B286C06B-0879-11D2-AACA-00C04FA33C20}')
df = arcpy.mapping.ListDataFrames(mxd)[0]
df.spatialReference = sr
arcpy.RefreshActiveView()

### done
