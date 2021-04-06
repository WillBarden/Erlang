create extension if not exists "uuid-ossp";

drop table if exists passwords;
drop table if exists auth_attempts;
drop table if exists password_changes;
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

create table permissions (
    name varchar(64) not null,
    primary key(name)
);
insert into permissions values ('ADMIN');

create table permission_assignments (
    user_id uuid not null,
    permission varchar(64) not null,
    foreign key(user_id) references users(id),
    foreign key(permission) references permissions(name)
);
