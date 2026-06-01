# Creates all directories required by the westside paper pipeline.
# Run this once before running any other scripts in this folder.

dirs <- c(
  "data/raw",
  "data/intermediate",
  "data/mst",
  "data/mst/for_paper",
  "output/westside_paper/ellipses/map_images_1940_65",
  "output/westside_paper/ellipses/density_maps",
  "output/westside_paper/data_vis", 
  "output/westside_paper/regressions"
)

for (d in dirs) {
  if (!dir.exists(d)) {
    dir.create(d, recursive = TRUE)
    message("Created: ", d)
  } else {
    message("Already exists: ", d)
  }
}
