create type account_status as ENUM('active', 'inactive', 'archived');

create table accounts (
  id serial primary key,
  name text not null,
  balance numeric(10, 2) not null,
  initial_balance numeric(10, 2) not null,
  currency text not null,
  status account_status not null,
  account_order integer not null,
  last_modified datetime not null
);
