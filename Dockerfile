FROM nginx
ADD nginx.conf /etc/nginx/conf.d/default.conf
ADD _site/ /var/www/
