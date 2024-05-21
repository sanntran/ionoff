package net.ionoff.center.server.persistence.dao;

import java.util.Collections;
import java.util.List;

import net.ionoff.center.server.entity.AreaCell;
import net.ionoff.center.server.entity.QueryCriteria;
import net.ionoff.center.server.persistence.dao.impl.AbstractGenericDao;
import org.springframework.stereotype.Repository;
import org.springframework.transaction.annotation.Transactional;

import net.ionoff.center.server.entity.Area;

import javax.persistence.Query;

@Repository
@Transactional
public class IAreaDao extends AbstractGenericDao<Area> {
    public IAreaDao() {
        setClass(Area.class);
    }

    private long countByName(long projectId, String name) {
        if (name == null || name.isEmpty()) {
            return countByProjectId(projectId);
        }
        String sql = "select count(area)"
                + " from Area as area"
                + " where area.project.id = :projectId"
                + " and lower(area.name) like :name";
        Query query = entityManager.createQuery(sql)
                .setParameter("projectId", projectId)
                .setParameter("name", "%" + name.toLowerCase() + "%");
        return countObjects(query);
    }

    private long countByProjectId(long projectId) {
        String sql = "select count(area)"
                + " from Area as area"
                + " where area.project.id = :projectId";
        Query query = entityManager.createQuery(sql)
                .setParameter("projectId", projectId);;
        return countObjects(query);
    }

    private List<Area> findByName(long projectId, String name,
                                  int fromIndex, int maxResults, String sortBy, boolean isAscending) {
        if (name == null || name.isEmpty()) {
            return findByProjectId(projectId, fromIndex, maxResults, isAscending);
        }
        String sql = "select distinct area"
                + " from Area as area"
                + " where area.project.id = :projectId"
                + " and lower(area.name) like :name"
                + " order by area." + sortBy;
        if (!isAscending) {
            sql = sql + " desc";
        }

        Query query = entityManager.createQuery(sql)
                .setParameter("projectId", projectId)
                .setParameter("name", "%" + name.toLowerCase() + "%");
        return findMany(query, fromIndex, maxResults);
    }

    private List<Area> findByProjectId(long projectId, int fromIndex, int maxResults, boolean isAscending) {
        String sql = "select distinct area"
                + " from Area as area"
                + " where area.project.id = :projectId"
                + " order by area.order, area.name";
        if (!isAscending) {
            sql = sql + " desc";
        }
        Query query = entityManager.createQuery(sql)
                .setParameter("projectId", projectId);
        return findMany(query, fromIndex, maxResults);
    }

    public List<Area> findByProjectId(long projectId) {
        String sql = "select distinct area"
                + " from Area as area"
                + " where area.project.id = :projectId"
                + " order by area.order, area.name";
        Query query = entityManager.createQuery(sql)
                .setParameter("projectId", projectId);
        return findMany(query);
    }

    public List<Area> findHavingAlertInProject(long projectId) {
        String sql =
                "SELECT DISTINCT a.* FROM ionoff.areas a" +
                " JOIN ionoff.zones z ON z.area_id = a.id" +
                " JOIN ionoff.sensors s ON s.zone_id = z.id" +
                " JOIN ionoff.sensors_status ss ON ss.id = s.id " +
                " WHERE a.project_id = :projectId AND ss.alert = 1";
        Query query = entityManager.createNativeQuery(sql, Area.class)
                .setParameter("projectId", projectId);
        return findMany(query);
    }

    @Override
    public long countByCriteria(QueryCriteria criteria) {
        if (NAME.equals(criteria.getSearchField())) {
            return countByName(criteria.getProjectId(), criteria.getSearchKey());
        }
        return 0;
    }

    @Override
    public List<Area> findByCriteria(QueryCriteria criteria) {
        if (NAME.equals(criteria.getSearchField())) {
            return findByName(criteria.getProjectId(), criteria.getSearchKey(),
                    criteria.getFromIndex(), criteria.getMaxResults(), criteria.getSortBy(), criteria.getIsAscending());
        }
        return Collections.emptyList();
    }

    public List<AreaCell> findForGridInProject(Long projectId) {
        String sql =
                "SELECT a.id," +
                        " a.name," +
                        " a.idx," +
                        " (SELECT COUNT(*) FROM ionoff.zones z" +
                            " WHERE z.area_id = a.id) as zone_count," +
                        " (SELECT COUNT(*) FROM ionoff.zones z" +
                            " JOIN ionoff.sensors s ON s.zone_id = z.id" +
                            " JOIN ionoff.sensors_status ss ON ss.id = s.id" +
                            " WHERE z.area_id = a.id AND ss.alert = 1) as alert_count" +
                " FROM ionoff.areas a" +
                " WHERE a.project_id = :projectId" +
                " ORDER BY a.idx";
        Query query = entityManager.createNativeQuery(sql, AreaCell.class)
                .setParameter("projectId", projectId);
        return query.getResultList();
    }
}
