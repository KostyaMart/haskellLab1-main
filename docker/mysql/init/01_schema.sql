CREATE TABLE IF NOT EXISTS resource_types (
  id INT AUTO_INCREMENT PRIMARY KEY,
  name VARCHAR(64) NOT NULL UNIQUE
);

CREATE TABLE IF NOT EXISTS resources (
  id INT AUTO_INCREMENT PRIMARY KEY,
  title VARCHAR(255) NOT NULL,
  type_id INT NOT NULL,
  abstract TEXT,
  purpose VARCHAR(255),
  opened_at DATE,
  usage_term_days INT,
  usage_terms TEXT,
  created_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
  updated_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP ON UPDATE CURRENT_TIMESTAMP,
  CONSTRAINT fk_resources_type FOREIGN KEY (type_id) REFERENCES resource_types(id)
);

CREATE TABLE IF NOT EXISTS authors (
  id INT AUTO_INCREMENT PRIMARY KEY,
  full_name VARCHAR(255) NOT NULL,
  dept VARCHAR(255)
);

CREATE TABLE IF NOT EXISTS resource_authors (
  resource_id INT NOT NULL,
  author_id INT NOT NULL,
  PRIMARY KEY (resource_id, author_id),
  CONSTRAINT fk_ra_r FOREIGN KEY (resource_id) REFERENCES resources(id) ON DELETE CASCADE,
  CONSTRAINT fk_ra_a FOREIGN KEY (author_id) REFERENCES authors(id) ON DELETE CASCADE
);

CREATE TABLE IF NOT EXISTS urls (
  id INT AUTO_INCREMENT PRIMARY KEY,
  resource_id INT NOT NULL,
  url VARCHAR(1024) NOT NULL,
  kind VARCHAR(64) DEFAULT 'homepage',
  CONSTRAINT fk_urls_r FOREIGN KEY (resource_id) REFERENCES resources(id) ON DELETE CASCADE
);

CREATE TABLE IF NOT EXISTS users (
  id INT AUTO_INCREMENT PRIMARY KEY,
  username VARCHAR(64) NOT NULL UNIQUE,
  email VARCHAR(255),
  role ENUM('student','staff','admin') DEFAULT 'student'
);

CREATE TABLE IF NOT EXISTS usage_events (
  id BIGINT AUTO_INCREMENT PRIMARY KEY,
  resource_id INT NOT NULL,
  user_id INT,
  event_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
  action ENUM('view','download','like','bookmark') NOT NULL,
  CONSTRAINT fk_ue_r FOREIGN KEY (resource_id) REFERENCES resources(id) ON DELETE CASCADE,
  CONSTRAINT fk_ue_u FOREIGN KEY (user_id) REFERENCES users(id) ON DELETE SET NULL
);

CREATE OR REPLACE VIEW v_resource_stats AS
SELECT
  r.id AS resource_id,
  r.title,
  COUNT(CASE WHEN ue.action = 'view' THEN 1 END) AS views,
  COUNT(CASE WHEN ue.action = 'download' THEN 1 END) AS downloads,
  COUNT(CASE WHEN ue.action = 'like' THEN 1 END) AS likes,
  COUNT(CASE WHEN ue.action = 'bookmark' THEN 1 END) AS bookmarks
FROM resources r
LEFT JOIN usage_events ue ON ue.resource_id = r.id
GROUP BY r.id, r.title;
