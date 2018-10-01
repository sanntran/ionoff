package net.ionoff.webhook.repository;


import net.ionoff.webhook.model.Center;
import org.springframework.data.repository.CrudRepository;
import org.springframework.stereotype.Repository;

import java.util.List;

@Repository
public interface CenterRepository extends CrudRepository<Center, Long> {

    List<Center> findByKey(String key);
}
