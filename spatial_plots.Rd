\name{plot_lake_map}
\alias{plot_lake_map}
\alias{classify_hotspots}
\alias{map_mp_hotspots}
\alias{plot_seasonality}
\alias{plot_morphology_profile}
\alias{plot_polymer_profile}
\alias{plot_depth_profile}
\title{Visualize freshwater microplastic patterns}
\description{Seasonal, composition, depth and coordinate-based point visualizations.}
\usage{
plot_lake_map(data, value_col = "MP_Mean")

classify_hotspots(data, value_col = "MP_Mean", method = c("quantile", "robust_z"))

map_mp_hotspots(data, value_col = "MP_Mean", method = c("quantile", "robust_z"))

plot_seasonality(data, lake = NULL, value_col = "MP_Mean")

plot_morphology_profile(data, id_col = "Lake_Name")

plot_polymer_profile(data, id_col = "Lake_Name")

plot_depth_profile(data, depth_col = "Depth_m", value_col = "MP_Mean", group_col = NULL)
}
\arguments{
\item{data}{A \code{limpid_db} or appropriate data frame.}
\item{value_col}{Numeric indicator column.}
\item{method}{Relative hotspot classification method.}
\item{lake}{Optional lake name or identifier filter.}
\item{id_col}{Identity/group column for profile bars.}
\item{depth_col}{Depth column in metres.}
\item{group_col}{Optional depth-profile grouping column.}
}
\value{A \pkg{ggplot2} object, except \code{classify_hotspots()} which returns a data frame.}
\details{Hotspot functions provide relative point classification and do not imply spatial interpolation. Depth plotting requires genuinely depth-resolved data and will reject the bundled event-level database.}
