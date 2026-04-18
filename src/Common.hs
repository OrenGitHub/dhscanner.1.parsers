module Common where

newtype SourceCodeFilePath = SourceCodeFilePath FilePath deriving ( Show, Eq )
newtype SourceCodeContent = SourceCodeContent String deriving ( Show, Eq )

data PathMapping
   = PathMapping
     {
         path_mapping_from :: String,
         path_mapping_to :: String
     }
     deriving ( Show, Eq )

data AdditionalRepoInfo
   = AdditionalRepoInfo
     {
         directories :: [ String ],
         filenames :: [ String ],
         optional_github_url :: Maybe String,
         path_mappings :: [ PathMapping ]
     }
     deriving ( Show, Eq )

