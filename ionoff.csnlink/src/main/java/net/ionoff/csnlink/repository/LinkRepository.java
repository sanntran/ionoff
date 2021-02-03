package net.ionoff.csnlink.repository;


import net.ionoff.csnlink.model.Link;
import org.springframework.data.jpa.repository.Query;
import org.springframework.data.repository.CrudRepository;
import org.springframework.data.repository.query.Param;
import org.springframework.stereotype.Repository;

import java.util.List;
import java.util.Optional;

@Repository
public interface LinkRepository extends CrudRepository<Link, Long> {

    @Query("SELECT link FROM Link link WHERE link.link = :link")
    Optional<Link> findByLink(@Param("link") String link);

    @Query("SELECT link FROM Link link WHERE link.status = :status")
    List<Link> findByStatus(@Param("status") Link.Status status);
}
