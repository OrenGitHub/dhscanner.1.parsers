module Common where

newtype SourceCodeFilePath = SourceCodeFilePath FilePath deriving ( Show, Eq )
newtype SourceCodeContent = SourceCodeContent String deriving ( Show, Eq )

data AdditionalRepoInfo
   = AdditionalRepoInfo
     {
         directories :: [ String ],
         filenames :: [ String ],
         optional_github_url :: Maybe String
     }
     deriving ( Show, Eq )

