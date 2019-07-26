Param(
  [string]$workingDir,
  [string]$gitHubUsername,
  [string]$gitHubAccessToken,
  [string]$repositoryOwner,
  [string]$repositoryName,
  [string]$branchName,
  [string]$sourceDir,
  [string]$publishType, # 'webapp' or 'data'
  [string]$gitHubEmail,
  [string]$commitMessage
)

function ExitOnError {
  if ($LASTEXITCODE -gt 0) { 
    throw "Command returned error code $LASTEXITCODE"
  }
}

$env:GIT_REDIRECT_STDERR = "2>&1"

$repoDir = Join-Path $workingDir "ghpages"
Write-Host "Cloning existing GitHub repository"
git clone "https://${gitHubUsername}:$gitHubAccessToken@github.com/$repositoryOwner/$repositoryName.git" "--branch=$branchName" $repoDir; ExitOnError
Write-Host "Removing existing files"
if ($publishType -eq 'webapp') {
    Get-ChildItem $repoDir -Exclude .git,api | Remove-Item -Recurse -Force
}
elseif ($publishType -eq 'data') {
    Get-ChildItem (Join-Path $repoDir "api")  | Remove-Item -Recurse -Force
}
else {
    throw "Invalid publish type: $publishType"
}
Write-Host "Copying new files"
Copy-Item (Join-Path $sourceDir "**") $repoDir -Force -Recurse
Push-Location $repoDir
try {
  git config user.email $gitHubEmail; ExitOnError
  git config user.name $gitHubUsername; ExitOnError
  Write-Host "Checking if commit is necessary"
  $isDirty = git status -s; ExitOnError
  if ($isDirty) {
    Write-Host "Work tree dirty ($isDirty), committing changes"
    git add .; ExitOnError
    git commit -m $commitMessage; ExitOnError
    git push; ExitOnError
  }
  else {
    Write-Host "Work tree clean, no commit necessary"
  }
}
finally {
  Pop-Location
}