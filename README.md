# Sport
Website for sporty activities.

|       | Web app | Data |
| ----- | ------------- | --------- |
| Build | [![Build Status](https://dev.azure.com/HTLVB/Sport/_apis/build/status/Build%20web%20app?branchName=master)](https://dev.azure.com/HTLVB/Sport/_build/latest?definitionId=1&branchName=master) | [![Build Status](https://dev.azure.com/HTLVB/Sport/_apis/build/status/Load%20data?branchName=master)](https://dev.azure.com/HTLVB/Sport/_build/latest?definitionId=2&branchName=master) |
| Deploy | [![Deploy Status](https://vsrm.dev.azure.com/HTLVB/_apis/public/Release/badge/6d7a0b3b-01dc-4f8b-8e82-8e3b2cdc1fd2/1/1)](https://dev.azure.com/HTLVB/Sport/_release?definitionId=1) | [![Deploy Status](https://vsrm.dev.azure.com/HTLVB/_apis/public/Release/badge/6d7a0b3b-01dc-4f8b-8e82-8e3b2cdc1fd2/2/2)](https://dev.azure.com/HTLVB/Sport/_release?definitionId=2) |

## Notes

### Trigger build in Azure DevOps
see https://docs.microsoft.com/en-us/rest/api/azure/devops/build/builds/queue?view=azure-devops-rest-5.0

```
POST https://dev.azure.com/HTLVB/Sport/_apis/build/builds?api-version=5.0
Authorization: Basic <username> <personal-access-token>
Content-Type: application/json

{"definition":{"id":2}}
```
