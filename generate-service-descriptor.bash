#!/bin/bash -e

MUSTANG_EXECUTABLE="${MUSTANG_EXECUTABLE:-./node_modules/mustang/lib/app.js}"
if [ ! -x "${MUSTANG_EXECUTABLE}" ]; then
   npm install mustang
fi

TEMPLATES_FILE="${TEMPLATES_FILE:-template-vars.csv}"
echo "RELEASE_VERSION" > "${TEMPLATES_FILE}"
echo "${RELEASE_VERSION}" >> "${TEMPLATES_FILE}"

SERVICE_DESCRIPTOR_TEMPLATE="${SERVICE_DESCRIPTOR_TEMPLATE:-service-descriptor.template.json}"
SERVICE_DESCRIPTOR="${SERVICE_DESCRIPTOR:-service-descriptor.json}"

${MUSTANG_EXECUTABLE} -t "${SERVICE_DESCRIPTOR_TEMPLATE}" -i "${TEMPLATES_FILE}" > "${SERVICE_DESCRIPTOR}"
