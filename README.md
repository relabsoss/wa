Goals
=====

This project was targetting on creation the starting point for web-developers for using Erlang + PostgreSQL + Redis as platform.

Installation quest
------------------

You need install PostgreSQL (9.4 or higher), Erlang (R18, we love maps). Redis currently not used, but we can. :)

Next - install and build application:

    git clone https://github.com/relabsoss/wa.git
    cd wa
    make
    ...
    (here we can get troubles with erlydtl. Just 'cd deps/erlydtl' and 'make' it)
    ...
    cp priv/sys.config priv/local.sys.config
    make run-local

... and its not work! :) We need some configuration steps:

Configure database:

    sudo su postgres -c psql
    postgres=# CREATE DATABASE wa ENCODING 'utf8';
    postgres=# CREATE USER wa WITH PASSWORD 'wa';
    postgres=# GRANT ALL PRIVILEGES ON DATABASE wa TO wa;
    postgres=# \q

Next, Mailgun. Go to https://mailgun.com/, register, add some domain, confirm it and get API access params - 
URL, public and private keys. Write it to local.sys.config at mail_sender section. (Some lyrics: most of public services are block templated messages as a spam.)

ReCAPTCHA 2 is using for auto-registration block. 
You need open and secret key for it. Go to https://www.google.com/recaptcha/admin#list, add your 
application, put localhost to allowed domains. 
Add key's pait to local.sys.config into 'recaptcha' param.

Next step - http://dadata.ru/ (data quality as a service). 
If you don't need its suggestions, just remove it code from /priv/www/index.html and /priv/www/js/app.js. 
Otherwise you must get your own personal API key from it site. 
Key must be writen to local.sys.config into 'dadata' section.

Now main external API is done. Lets go for OAuth providers.
* Facebook: register application for web on https://developers.facebook.com/apps and add localhost to approoved domains
* Google: go to https://console.developers.google.com/project, add new project, then go to "APIs & auth -> Credetials" and create new client ID. 
Set up Redirect URIs to http://localhost:8080/oauth/google/callback, take client ID and secret to local.sys.config.
* VKontakte: https://vk.com/apps?act=manage, create application as web-site and set up URL to http://localhost:8080. Take id and key for config.

Almost done? Yep.

    make run-local
    1> persist:init_db(pgdb).

Now you can try to open http://localhost:8080 in your browser. Must works.
