import System.Environment

serviceDescriptor :: String -> String
serviceDescriptor version = "{\n\
\   \"description\": \"Import files and stuff\",\n\
\   \"name\": \"Confluence Pandoc Connect\",\n\
\   \"organization\": \"RD:Identity\",\n\
\   \"externalAccess\": true,\n\
\   \"resources\": [{\n\
\      \"type\": \"postgres-db\",\n\
\      \"name\": \"confluence-pandoc-connect\"\n\
\   }],\n\
\   \"links\": {\n\
\      \"binary\": {\n\
\         \"name\": \"docker.atlassian.io/atlassian/confluence-pandoc-connect\",\n\
\         \"type\": \"docker\",\n\
\         \"tag\": \"" ++ version ++ "\"\n\
\      },\n\
\      \"healthcheck\": {\n\
\         \"uri\": \"rest/heartbeat\"\n\
\      },\n\
\      \"source\": {\n\
\         \"url\": \"https://bitbucket.org/atlassianlabs/confluence-pandoc-connect\"\n\
\      }\n\
\   },\n\
\   \"owners\": [\n\
\      \"aknoll@atlassian.com\"\n\
\   ],\n\
\   \"notifications\": {\n\
\      \"pagerduty\": {\n\
\         \"cloudwatch\": \"https://events.pagerduty.com/adapter/cloudwatch_sns/v1/124e0f010f214a9b9f30b768e7b18e69\",\n\
\         \"apiKey\": \"5d11612f25b840faaf77422edeff9c76\"\n\
\      },\n\
\      \"email\": \"aknoll@atlassian.com\"\n\
\   },\n\
\   \"scaling\": {\n\
\      \"min\": 2\n\
\   },\n\
\   \"downstreamServices\": []\n\
\}"

versionEnvName = "RELEASE_VERSION"
descriptorFileName = "service-descriptor.json"

main = do
  ver <- getEnv versionEnvName
  writeFile descriptorFileName (serviceDescriptor ver)
