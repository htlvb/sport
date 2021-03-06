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

# https://stackoverflow.com/a/55422946/1293659
$env:GIT_REDIRECT_STDERR = "2>&1"

$repoDir = Join-Path $workingDir "ghpages"
Write-Host "Cloning existing GitHub repository"
git clone "https://${gitHubUsername}:$gitHubAccessToken@github.com/$repositoryOwner/$repositoryName.git" "--branch=$branchName" $repoDir; ExitOnError
if ($publishType -eq 'webapp') {
    Write-Host "Removing existing files"
    Get-ChildItem $repoDir -Exclude .git,api | Remove-Item -Recurse -Force
    Write-Host "Copying new files"
    Copy-Item (Join-Path $sourceDir "**") $repoDir -Force -Recurse
}
elseif ($publishType -eq 'data') {
    Write-Host "Removing existing files"
    Get-ChildItem (Join-Path $repoDir "api")  | Remove-Item -Recurse -Force
    Write-Host "Copying new files"
    Copy-Item (Join-Path $sourceDir "**") "$repoDir\api" -Force -Recurse
}
else {
    throw "Invalid publish type: $publishType"
}
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