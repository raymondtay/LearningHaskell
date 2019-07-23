
module Charts (plotChart) where

-- need the Chart, Chart-diagrams package
import Data.Foldable (traverse_, toList)
import Graphics.Rendering.Chart.Easy (plot, line, (.=), layout_title)
import Graphics.Rendering.Chart.Backend.Diagrams (toFile, loadSansSerifFonts, FileOptions(..), FileFormat(SVG))
import QuoteData

plotChart :: (Functor t, Foldable t) => String -> t QuoteData -> [QField] -> FilePath -> IO ()
plotChart title quotes qfs fname = toFile fileOptions fname $ do
  layout_title .= title -- title for the chart
  traverse_ plotLine qfs -- plot a line for each field
    where
      fileOptions = FileOptions (800, 600) SVG loadSansSerifFonts -- specify options
      plotLine qf = plot $ line (show qf) [toList $ fmap (qf2pd qf) quotes] -- plot 1 line
      qf2pd qf q = (day q, realToFrac $ field2fun qf q :: Double)

