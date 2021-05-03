create extension if not exists "uuid-ossp";

drop table if exists passwords;
drop table if exists chat_messages;
drop table if exists auth_attempts;
drop table if exists password_changes;

drop table if exists chats_table;
drop table if exists tables;
drop table if exists permission_assignments;
drop table if exists permissions;


drop table if exists users;

create table users (
    id uuid not null default uuid_generate_v1(),
    username varchar(64) not null,
    created timestamptz not null default now(),
    primary key(id),
    unique(username)
);

create table passwords (
    user_id uuid not null,
    salt char(88) not null,
    hash char(88) not null,
    foreign key(user_id) references users(id)
);

create table auth_attempts (
    user_id uuid not null,
    successful boolean not null,
    at timestamptz not null default now(),
    foreign key(user_id) references users(id)
);

create table password_changes (
    user_id uuid not null,
    at timestamptz not null default now(),
    foreign key(user_id) references users(id)
);

create sequence if not exists permissions_seq as smallint cache 8;
create table permissions (
    id smallint not null default nextval('permissions_seq'),
    name varchar(64) not null,
    primary key(name)
);
insert into permissions (name) values ('ADMIN');

create table permission_assignments (
    user_id uuid not null,
    permission varchar(64) not null,
    foreign key(user_id) references users(id),
    foreign key(permission) references permissions(name),
    check(permission = upper(permission))
);

create sequence if not exists tables_seq as integer cache 8;
create table tables (
    id integer not null default nextval('tables_seq'),
    owner uuid not null,
    name varchar(256) not null,
    seats smallint not null default 8,
    primary key(id),
    foreign key(owner) references users(id),
    unique(owner, name),
    check(seats >= 2 and seats <= 10)
);

create sequence if not exists chat_messages_seq as integer cache 64;
create table chat_messages (
    id integer not null default nextval('chat_messages_seq'),
    table_id integer not null,
    sender uuid not null,
    msg varchar(2048) not null,
    primary key(id),
    foreign key(table_id) references tables(id),
    foreign key(sender) references users(id)
);
