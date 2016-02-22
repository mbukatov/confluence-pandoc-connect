plan(key:'CPCD',name:'Confluence Pandoc Connect (docker)') {
   project(key:'ACA',name:'Atlassian Connect Add-ons')

   repository(name:'Confluence Pandoc Connect')

   variable(key:'stack.binary.path',value:'.stack-work/install/x86_64-linux/lts-5.1/7.10.3/bin')

   trigger(type:'polling',description:'Polling',
      strategy:'periodically',
      frequency:'120') {
      repository(name:'Confluence Pandoc Connect')
   }

   notification(type:'All Builds Completed',recipient:'watchers')

   notification(type:'All Builds Completed',
      recipient:'hipchat',
      apiKey:'${bamboo.atlassian.hipchat.apikey.password}',
      notify:'true',
      room:'2116917')

   stage(name:'Build Docker Image',description:'Create a production ready docker image.') {
      job(key:'CDI',name:'Build and push docker image') {
         requirement(key:'elastic',condition:'equals',value:'true')

         requirement(key:'os',condition:'equals',value:'Linux')

         artifactDefinition(name:'tag_variables',pattern:'*_variables',shared:'true')

         task(type:'checkout',description:'Checkout default repository') {
         }

         task(type:'script',description:'Create variables file',
            scriptBody:'''
FILENAME="tag_variables"
IMAGE_TAG=$(git describe --always)
IMAGE_NAME="atlassian/confluence-pandoc-connect"
echo "image.tag=${IMAGE_TAG}" >> ${FILENAME}
echo "image.name=${IMAGE_NAME}" >> ${FILENAME}
echo "image.full.path=docker.atlassian.io/${IMAGE_NAME}:${IMAGE_TAG}" >> ${FILENAME}
''')

         task(type:'injectBambooVariables',description:'Inject variables',
            namespace:'inject',scope:'LOCAL',filePath:'tag_variables')

         task(type:'custom',createTaskKey:'com.atlassian.bamboo.plugins.bamboo-docker-plugin:task.docker.cli',
            description:'Build binary',
            commandOption:'build',
            repository:'cpc-build',
            registryOption:'hub',
            serviceUrlPattern:'http://localhost:${docker.port}',
            dockerfileOption:'existing',
            workDir:'/data',
            containerDataVolume_0:'/data',
            hostDirectory_0:'${bamboo.working.directory}')

         task(type:'custom',createTaskKey:'com.atlassian.bamboo.plugins.bamboo-docker-plugin:task.docker.cli',
            description:'Run build image',
            commandOption:'run',
            registryOption:'hub',
            serviceUrlPattern:'http://localhost:${docker.port}',
            workDir:'/data',
            containerDataVolume_0:'/data',
            hostDirectory_0:'${bamboo.working.directory}',
            dockerfileOption:'inline',
            image:'cpc-build',
            command:'cp /build/${bamboo.stack.binary.path}/confluence-pandoc-connect /data/confluence-pandoc-connect',
            name:'cpc-build-container')

         task(type:'custom',createTaskKey:'com.atlassian.bamboo.plugins.bamboo-docker-plugin:task.docker.cli',
            description:'Build run image',
            serviceTimeout:'120',
            commandOption:'build',
            repository:'docker.atlassian.io/atlassian/confluence-pandoc-connect:${bamboo.inject.image.tag}',
            registryOption:'hub',
            serviceUrlPattern:'http://localhost:${docker.port}',
            workDir:'/data',
            containerDataVolume_0:'/data',
            hostDirectory_0:'${bamboo.working.directory}',
            dockerfileOption:'inline',
            dockerfile:'''
FROM fpco/stack-run
MAINTAINER Avi Knoll <aknoll@atlassian.com>

# Expose the default port required by Micros
EXPOSE 8080

# Install Java
RUN apt-get update && apt-get install -y openjdk-7-jre-headless

# Install static files
COPY snaplets /service/snaplets
COPY resources /service/resources
COPY migrations /service/migrations
COPY static /service/static

# Copy and run binary
ADD ./confluence-pandoc-connect /usr/bin/confluence-pandoc-connect
WORKDIR /service
CMD ["confluence-pandoc-connect", "--access-log=-", "--error-log=-", "--port=8080"]
''')

         task(type:'custom',createTaskKey:'com.atlassian.bamboo.plugins.bamboo-docker-plugin:task.docker.cli',
            description:'Push run image',serviceTimeout:'120',
            commandOption:'push',registryOption:'custom',
            serviceUrlPattern:'http://localhost:${docker.port}',
            workDir:'/data',containerDataVolume_0:'/data',
            dockerfileOption:'inline',hostDirectory_0:'${bamboo.working.directory}',
            pushRepository:'docker.atlassian.io/atlassian/confluence-pandoc-connect:${bamboo.inject.image.tag}')

      }
   }
   branchMonitoring() {
      createBranch()
      inactiveBranchCleanup(periodInDays:'30')
      deletedBranchCleanup(periodInDays:'30')
   }
}
