UPDATE `relays` SET `label` = (
    SELECT DISTINCT device.name FROM `devices` AS device WHERE device.id = device_id)
    WHERE `device_id` IS NOT NULL;
