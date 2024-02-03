# Stage 1
FROM node:21.4-bullseye-slim as builder
WORKDIR /app
COPY package.json .
COPY yarn.lock .
RUN yarn install
COPY . .

# Replace placeholder with actual API endpoint
ARG REACT_APP_KISSA_API_ENDPOINT=https://kissa-api.jollymoss-4112728e.uksouth.azurecontainerapps.io
RUN sed -i "s|placeholder|${REACT_APP_KISSA_API_ENDPOINT}|g" src/*.js


RUN yarn build

#Stage 2
FROM nginx:1.25.3-alpine-slim

# Remove the default Nginx configuration
RUN rm /etc/nginx/conf.d/default.conf
COPY nginx.conf /etc/nginx/conf.d/

WORKDIR /usr/share/nginx/html
RUN rm -rf ./*
COPY --from=builder /app/build .
EXPOSE 3000
ENTRYPOINT ["nginx", "-g", "daemon off;"]


