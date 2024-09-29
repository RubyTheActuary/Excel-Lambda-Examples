This project demonstrates the power of Excel 365's new:
1. Lambda function and its helper - https://support.microsoft.com/en-us/office/lambda-function-bd212d27-1cd1-4321-a34a-ccbf254b8b67,
2. Let statements - https://www.thespreadsheetguru.com/excel-let-function/, and
3. Dynamic Arrays - https://www.thespreadsheetguru.com/dynamic-array-function-list/.

As part of this, I also show how to create a lambda library using the Advance Formula Environment (AFE) https://www.microsoft.com/en-us/garage/profiles/excel-labs/. AFE allows users to download gists from GitHub. There are two gists associated with this project: Utilities and Domain. 

The utility gist is https://gist.github.com/RubyTheActuary/46ee2cacdb49e772da5ef4050b5195ee. 
The domain gists is https://gist.github.com/RubyTheActuary/cd905e4a97ae6ba88584449b930b57df. 

Utilities are fancy recursive loops that handle problems that the SCAN function, https://support.microsoft.com/en-us/office/scan-function-d58dfd11-9969-4439-b2dc-e7062724de29, cannot handle. The domain contains the specific library functions that have to do with modeling the variable annuity domain of the AIG Polaris Max product. 

The Excel spreadsheets take two products displayed in AIG's Polaris Max prospectus D. Once the product is from examples 1 - 7. The second product is examples 8+. One sheet shows how to code the product using classical Excel cell references. The second sheet shows how to transform the cell references using the Map function, https://support.microsoft.com/en-us/office/map-function-48006093-f97c-47c1-bfcc-749263bb1f01, so that the logic can be separated from the cell references using Lambda. The third sheet shows how to use only lambda functions and dynamic arrays. This transformation may seem trivial, but it is pretty involved. Classical Excel is much more of a procedural paradigm mentality. Using strictly lambda functions and dynamic arrays requires a fully functional paradigm like Haskell, https://www.haskell.org/. 

The HaskellVa folder contains a visual studio code project written in Haskell that replicates the VaAigModelLambdaExamplePresentationv2.xlsx and the ExamplePolarisMaxDaily.xlsx spreadsheets. If you want to install Haskell on your box, this is a great video: https://www.youtube.com/watch?v=vn1IOxVplKQ. I would recommend stopping at 7:15. At that time, I would recommend watching this video https://www.youtube.com/watch?v=-DHEmrKhjCM. The first video installation projects used Stack, a wrapper around cabal. The second uses cabal directly, which I found more manageable, and many videos recommended. I have found it easier to use because I am a beginner at Haskell. Plus, other videos highlight that Stack was to remove deficiencies in the Cabal, but now the Cabal has been improved enough that Stack is no longer needed. Once you have installed Haskell, you can run the project in Powershell by typing cabal run HaskellVa.

Once the logic is transformed to Lambda Logic, ChatGPT, https://chatgpt.com/, can transform the lambda language to any programming language. The rows represent a for loop. The columns from left to right represent function calls from bottom to top within the for loop.
