--------------------------------------------    2017         -------------------------------------------------

--------------------------------------------  DISPONIBILIDAD ------------------------------------------------- 
select 'Disponibilidad vs Modificado - Precomprometido - Comprometido - Devengado - Ejercido - Pagado)' AS Mensaje,
       COUNT(*) as Registros FROM (

SELECT nIdClaveIngresos, nIdClaveEgresos, SUM(mModificado) as mModificado,  SUM(mPrecomprometido) as mPrecomprometido, SUM(mComprometido) as mComprometido,  SUM(mDevengado) as mDevengado,  SUM(mEjercido) as mEjercido, SUM(mPagado) as mPagado, SUM(mDisponible) as mDisponible FROM
(
SELECT nIdClaveIngresos, nIdClaveEgresos, mTotal as mModificado, 0.00 as mPrecomprometido, 0.00 as mComprometido, 0.00 as mDevengado, 0.00 as mEjercido, 0.00 as mPagado, 0.00 as mDisponible from eClavePresupuestariaInternaModificado
UNION ALL
SELECT nIdClaveIngresos, nIdClaveEgresos, 0.00 as mModificado, mTotal as mPrecomprometido, 0.00 as mComprometido, 0.00 as mDevengado, 0.00 as mEjercido, 0.00 as mPagado, 0.00 as mDisponible from eClavePresupuestariaInternaPrecomprometido 
UNION ALL
SELECT nIdClaveIngresos, nIdClaveEgresos, 0.00 as mModificado, 0.00 as mPrecomprometido, mTotal as mComprometido, 0.00 as mDevengado, 0.00 as mEjercido, 0.00 as mPagado, 0.00 as mDisponible from eClavePresupuestariaInternaComprometido 
UNION ALL
SELECT nIdClaveIngresos, nIdClaveEgresos, 0.00 as mModificado, 0.00 as mPrecomprometido, 0.00 as mComprometido, mTotal as mDevengado, 0.00 as mEjercido, 0.00 as mPagado, 0.00 as mDisponible from eClavePresupuestariaInternaDevengado 
UNION ALL
SELECT nIdClaveIngresos, nIdClaveEgresos, 0.00 as mModificado, 0.00 as mPrecomprometido, 0.00 as mComprometido, 0.00 as mDevengado, mTotal as mEjercido, 0.00 as mPagado, 0.00 as mDisponible from eClavePresupuestariaInternaEjercido 
UNION ALL
SELECT nIdClaveIngresos, nIdClaveEgresos, 0.00 as mModificado, 0.00 as mPrecomprometido, 0.00 as mComprometido, 0.00 as mDevengado, 0.00 as mEjercido, mTotal as mPagado, 0.00 as mDisponible from eClavePresupuestariaInternaPagado 
UNION ALL
SELECT nIdClaveIngresos, nIdClaveEgresos, 0.00 as mModificado, 0.00 as mPrecomprometido, 0.00 as mComprometido, 0.00 as mDevengado, 0.00 as mEjercido, 0.00 as mPagado, mTotal as mDisponible from eClavePresupuestariaInternaDisponible
) as a
GROUP BY nIdClaveIngresos, nIdClaveEgresos
HAVING SUM(mDisponible) <>  (SUM(mModificado) - SUM(mPrecomprometido)  - SUM(mComprometido) - SUM(mDevengado) - SUM(mEjercido) - SUM(mPagado))  
) as a
------------------------------------------  ANALITICO VS DEVENGADO ACUMULADO ---------------------------------
SELECT 'Enero' as Mes, COUNT(*) as Registros FROM (
select a.mEnero - b.mEnero as dif
from (select nIdClaveIngresos,nIdClaveEgresos,sum(mClave) AS mEnero from tDocumentoPagoAnalitico() AS A
where fRegistro BETWEEN '20170101' and '20170201'
GROUP BY  nIdClaveIngresos,nIdClaveEgresos) AS a
inner join eClavePresupuestariaInternaDevengadoAcumulado  b on a.nIdClaveIngresos = b.nIdClaveIngresos
                                                             and a.nIdClaveEgresos = b.nIdClaveEgresos  
WHERE  a.mEnero <> b.mEnero
) as b

UNION ALL
SELECT 'Febrero' as Mes, COUNT(*) as Registros FROM (
select a.mFebrero - b.mFebrero as dif
from (select nIdClaveIngresos,nIdClaveEgresos,sum(mClave) AS mFebrero from tDocumentoPagoAnalitico() AS A
where fRegistro BETWEEN '20170201' and '20170301'
GROUP BY  nIdClaveIngresos,nIdClaveEgresos) AS a
inner join eClavePresupuestariaInternaDevengadoAcumulado  b on a.nIdClaveIngresos = b.nIdClaveIngresos
                                                             and a.nIdClaveEgresos = b.nIdClaveEgresos  
WHERE  a.mFebrero <> b.mFebrero
) as b

UNION ALL
SELECT 'Marzo' as Mes, COUNT(*) as Registros FROM (
select a.mMarzo - b.mMarzo as dif
from (select nIdClaveIngresos,nIdClaveEgresos,sum(mClave) AS mMarzo from tDocumentoPagoAnalitico() AS A
where fRegistro BETWEEN '20170301' and '20170401'
GROUP BY  nIdClaveIngresos,nIdClaveEgresos) AS a
inner join eClavePresupuestariaInternaDevengadoAcumulado  b on a.nIdClaveIngresos = b.nIdClaveIngresos
                                                             and a.nIdClaveEgresos = b.nIdClaveEgresos  
WHERE  a.mMarzo <> b.mMarzo
) as b

UNION ALL
SELECT 'Abril' as Mes, COUNT(*) as Registros FROM (
select a.mAbril - b.mAbril as dif
from (select nIdClaveIngresos,nIdClaveEgresos,sum(mClave) AS mAbril from tDocumentoPagoAnalitico() AS A
where fRegistro BETWEEN '20170401' and '20170501'
GROUP BY  nIdClaveIngresos,nIdClaveEgresos) AS a
inner join eClavePresupuestariaInternaDevengadoAcumulado  b on a.nIdClaveIngresos = b.nIdClaveIngresos
                                                             and a.nIdClaveEgresos = b.nIdClaveEgresos  
WHERE  a.mAbril <> b.mAbril
) as b

UNION ALL
SELECT 'Mayo' as Mes, COUNT(*) as Registros FROM (
select a.mMayo - b.mMayo as dif
from (select nIdClaveIngresos,nIdClaveEgresos,sum(mClave) AS mMayo from tDocumentoPagoAnalitico() AS A
where fRegistro BETWEEN '20170501' and '20170601'
GROUP BY  nIdClaveIngresos,nIdClaveEgresos) AS a
inner join eClavePresupuestariaInternaDevengadoAcumulado  b on a.nIdClaveIngresos = b.nIdClaveIngresos
                                                             and a.nIdClaveEgresos = b.nIdClaveEgresos  
WHERE  a.mMayo <> b.mMayo
) as b

UNION ALL
SELECT 'Junio' as Mes, COUNT(*) as Registros FROM (
select a.mJunio - b.mJunio as dif
from (select nIdClaveIngresos,nIdClaveEgresos,sum(mClave) AS mJunio from tDocumentoPagoAnalitico() AS A
where fRegistro BETWEEN '20170601' and '20170701'
GROUP BY  nIdClaveIngresos,nIdClaveEgresos) AS a
inner join eClavePresupuestariaInternaDevengadoAcumulado  b on a.nIdClaveIngresos = b.nIdClaveIngresos
                                                             and a.nIdClaveEgresos = b.nIdClaveEgresos  
WHERE a.mJunio <> b.mJunio
) as b

UNION ALL
SELECT 'Julio' as Mes, COUNT(*) as Registros FROM (
select a.mJulio - b.mJulio as dif
from (select nIdClaveIngresos,nIdClaveEgresos,sum(mClave) AS mJulio from tDocumentoPagoAnalitico() AS A
where fRegistro BETWEEN '20170701' and '20170801'
GROUP BY  nIdClaveIngresos,nIdClaveEgresos) AS a
inner join eClavePresupuestariaInternaDevengadoAcumulado  b on a.nIdClaveIngresos = b.nIdClaveIngresos
                                                             and a.nIdClaveEgresos = b.nIdClaveEgresos  
WHERE a.mJulio <> b.mJulio
) as b

UNION ALL
SELECT 'Agosto' as Mes, COUNT(*) as Registros FROM (
select a.mAgosto - b.mAgosto as dif
from (select nIdClaveIngresos,nIdClaveEgresos,sum(mClave) AS mAgosto from tDocumentoPagoAnalitico() AS A
where fRegistro BETWEEN '20170801' and '20170901'
GROUP BY  nIdClaveIngresos,nIdClaveEgresos) AS a
inner join eClavePresupuestariaInternaDevengadoAcumulado  b on a.nIdClaveIngresos = b.nIdClaveIngresos
                                                             and a.nIdClaveEgresos = b.nIdClaveEgresos  
WHERE a.mAgosto <> b.mAgosto
) as b

UNION ALL
SELECT 'Septiembre' as Mes, COUNT(*) as Registros FROM (
select a.mSeptiembre - b.mSeptiembre as dif
from (select nIdClaveIngresos,nIdClaveEgresos,sum(mClave) AS mSeptiembre from tDocumentoPagoAnalitico() AS A
where fRegistro BETWEEN '20170901' and '20171001'
GROUP BY  nIdClaveIngresos,nIdClaveEgresos) AS a
right join eClavePresupuestariaInternaDevengadoAcumulado  b on a.nIdClaveIngresos = b.nIdClaveIngresos
                                                             and a.nIdClaveEgresos = b.nIdClaveEgresos  
WHERE a.mSeptiembre <> b.mSeptiembre
) as b

UNION ALL
SELECT 'Octubre' as Mes, COUNT(*) as Registros FROM (
select a.mOctubre - b.mOctubre as dif
from (select nIdClaveIngresos,nIdClaveEgresos,sum(mClave) AS mOctubre from tDocumentoPagoAnalitico() AS A
where fRegistro BETWEEN '20171001' and '20171101'
GROUP BY  nIdClaveIngresos,nIdClaveEgresos) AS a
right join eClavePresupuestariaInternaDevengadoAcumulado  b on a.nIdClaveIngresos = b.nIdClaveIngresos
                                                             and a.nIdClaveEgresos = b.nIdClaveEgresos  
WHERE a.mOctubre <> b.mOctubre
) as b

UNION ALL
SELECT 'Noviembre' as Mes, COUNT(*) as Registros FROM (
select a.mNoviembre - b.mNoviembre as dif
from (select nIdClaveIngresos,nIdClaveEgresos,sum(mClave) AS mNoviembre from tDocumentoPagoAnalitico() AS A
where fRegistro BETWEEN '20171101' and '20171201'
GROUP BY  nIdClaveIngresos,nIdClaveEgresos) AS a
right join eClavePresupuestariaInternaDevengadoAcumulado  b on a.nIdClaveIngresos = b.nIdClaveIngresos
                                                             and a.nIdClaveEgresos = b.nIdClaveEgresos  
WHERE a.mNoviembre <> b.mNoviembre
) as b

UNION ALL
SELECT 'Diciembre' as Mes, COUNT(*) as Registros FROM (
select a.mDiciembre - b.mDiciembre as dif
from (select nIdClaveIngresos,nIdClaveEgresos,sum(mClave) AS mDiciembre from tDocumentoPagoAnalitico() AS A
where fRegistro BETWEEN '20171201' and '20180101'
GROUP BY  nIdClaveIngresos,nIdClaveEgresos) AS a
right join eClavePresupuestariaInternaDevengadoAcumulado  b on a.nIdClaveIngresos = b.nIdClaveIngresos
                                                             and a.nIdClaveEgresos = b.nIdClaveEgresos  
WHERE a.mDiciembre <> b.mDiciembre
) as b 

------------------------------------------------------- INCONSISTENCIAS CONTABILIDAD ---------------------------------------------
----------------------------- 01 detalle cuenta contable Almacen, Bancos y RFC no corresponde con Detalle Movimiento   ----------------------------
SELECT '01 Inconsistencia detalle movimiento con cuenta contable' as Tipo,  COUNT(*) FROM 
(
	SELECT 'Almacen' as Inconsistente, c.lHayAlmacen, c.lHayBanco, c.lHayRfc, b.cIdOrigenPoliza, a.* 
	  FROM [cDetalleMovimientoPoliza] a
	  LEFT JOIN [cMaestroPoliza] b ON
		   a.cEjercicio = b.cEjercicio AND 
		   a.cIdEntidadContable = b.cIdEntidadContable AND 
		   a.cIdTipoPoliza = b.cIdTipoPoliza AND
		   a.nPoliza = b.nPoliza AND 
		   a.cAjuste = b.cAjuste
	  LEFT JOIN cCatalogoCuentaContable c ON 
		   a.cIdCuentaContable = c.cIdCuentaContable     
	  WHERE c.lHayAlmacen = 1 AND a.cIdDetalleCuenta <> 'A' 
	UNION ALL  
	SELECT 'Banco' as Inconsistente, c.lHayAlmacen, c.lHayBanco, c.lHayRfc, b.cIdOrigenPoliza, a.* 
	  FROM [cDetalleMovimientoPoliza] a
	  LEFT JOIN [cMaestroPoliza] b ON
		   a.cEjercicio = b.cEjercicio AND 
		   a.cIdEntidadContable = b.cIdEntidadContable AND 
		   a.cIdTipoPoliza = b.cIdTipoPoliza AND
		   a.nPoliza = b.nPoliza AND 
		   a.cAjuste = b.cAjuste
	  LEFT JOIN cCatalogoCuentaContable c ON 
		   a.cIdCuentaContable = c.cIdCuentaContable     
	  WHERE c.lHayBanco = 1 AND a.cIdDetalleCuenta <> 'B' 
	 UNION ALL  
	 SELECT 'RFC' as Inconsistente, c.lHayAlmacen, c.lHayBanco, c.lHayRfc, b.cIdOrigenPoliza, a.* 
	  FROM [cDetalleMovimientoPoliza] a
	  LEFT JOIN [cMaestroPoliza] b ON
		   a.cEjercicio = b.cEjercicio AND 
		   a.cIdEntidadContable = b.cIdEntidadContable AND 
		   a.cIdTipoPoliza = b.cIdTipoPoliza AND
		   a.nPoliza = b.nPoliza AND 
		   a.cAjuste = b.cAjuste
	  LEFT JOIN cCatalogoCuentaContable c ON 
		   a.cIdCuentaContable = c.cIdCuentaContable     
	  WHERE c.lHayRFC = 1 AND a.cIdDetalleCuenta <> 'R'  
	 UNION ALL  
	 SELECT 'CABM' as Inconsistente, c.lHayAlmacen, c.lHayBanco, c.lHayRfc, b.cIdOrigenPoliza, a.* 
	  FROM [cDetalleMovimientoPoliza] a
	  LEFT JOIN [cMaestroPoliza] b ON
		   a.cEjercicio = b.cEjercicio AND 
		   a.cIdEntidadContable = b.cIdEntidadContable AND 
		   a.cIdTipoPoliza = b.cIdTipoPoliza AND
		   a.nPoliza = b.nPoliza AND 
		   a.cAjuste = b.cAjuste
	  LEFT JOIN cCatalogoCuentaContable c ON 
		   a.cIdCuentaContable = c.cIdCuentaContable     
	  WHERE c.lHayCABM = 1 AND a.cIdDetalleCuenta <> 'M'    
	UNION ALL   
	 SELECT 'Sin detalle' as Inconsistente, c.lHayAlmacen, c.lHayBanco, c.lHayRfc, b.cIdOrigenPoliza, a.* 
	  FROM [cDetalleMovimientoPoliza] a
	  LEFT JOIN [cMaestroPoliza] b ON
		   a.cEjercicio = b.cEjercicio AND 
		   a.cIdEntidadContable = b.cIdEntidadContable AND 
		   a.cIdTipoPoliza = b.cIdTipoPoliza AND
		   a.nPoliza = b.nPoliza AND 
		   a.cAjuste = b.cAjuste
	  LEFT JOIN cCatalogoCuentaContable c ON 
		   a.cIdCuentaContable = c.cIdCuentaContable     
	  WHERE (c.lHayAlmacen = 0 AND c.lHayBanco = 0 AND c.lHayRFC = 0 AND c.lHayCABM = 0) AND a.cIdDetalleCuenta <> 'S'    
	  --ORDER BY 13,12,8,5,9
) as a

UNION ALL
----------------------------- 02 Detalle Movimiento sin Detalle Movimiento Almacen, Bancos o RFC correspondiente ----------------------------
SELECT '02 detalle movimiento sin detalle ABRM' as Tipo,  COUNT(*) FROM 
(
----------------------------------------------------------- ALMACEN -----------------------------------------------
SELECT 'Almacen' as Faltante, b.cIdOrigenPoliza, a.* 
  FROM [cDetalleMovimientoPoliza] a
  LEFT JOIN [cMaestroPoliza] b ON
       a.cEjercicio = b.cEjercicio AND 
       a.cIdEntidadContable = b.cIdEntidadContable AND 
       a.cIdTipoPoliza = b.cIdTipoPoliza AND
       a.nPoliza = b.nPoliza AND 
       a.cAjuste = b.cAjuste
  WHERE a.cIdDetalleCuenta = 'A' AND
		 b.cIdEstadoPoliza NOT IN ('C','N') AND 
		a.cEjercicio + a.cIdEntidadContable + a.cIdTipoPoliza + CAST(a.nPoliza as CHAR(5)) + a.cAjuste + CAST(a.nMovimiento AS CHAR(4)) NOT IN
		(SELECT cEjercicio + cIdEntidadContable + cIdTipoPoliza + CAST(nPoliza as CHAR(5)) + cAjuste + CAST(nMovimiento AS CHAR(4))
		 FROM [cDetalleMovimientoPolizaAlmacen])
UNION ALL 		 
----------------------------------------------------------- BANCO -----------------------------------------------
SELECT 'Banco' as Faltante, b.cIdOrigenPoliza, a.* 
  FROM [cDetalleMovimientoPoliza] a
  LEFT JOIN [cMaestroPoliza] b ON
       a.cEjercicio = b.cEjercicio AND 
       a.cIdEntidadContable = b.cIdEntidadContable AND 
       a.cIdTipoPoliza = b.cIdTipoPoliza AND
       a.nPoliza = b.nPoliza AND 
       a.cAjuste = b.cAjuste
  WHERE a.cIdDetalleCuenta = 'B' AND
        b.cIdEstadoPoliza NOT IN ('C','N') AND 
		a.cEjercicio + a.cIdEntidadContable + a.cIdTipoPoliza + CAST(a.nPoliza as CHAR(5)) + a.cAjuste + CAST(a.nMovimiento AS CHAR(4)) NOT IN
		(SELECT cEjercicio + cIdEntidadContable + cIdTipoPoliza + CAST(nPoliza as CHAR(5)) + cAjuste + CAST(nMovimiento AS CHAR(4))
		 FROM [cDetalleMovimientoPolizaBanco])

UNION ALL 
----------------------------------------------------------- CABM -----------------------------------------------
SELECT 'Cabm' as Faltante, b.cIdOrigenPoliza, a.* 
  FROM [cDetalleMovimientoPoliza] a
  LEFT JOIN [cMaestroPoliza] b ON
       a.cEjercicio = b.cEjercicio AND 
       a.cIdEntidadContable = b.cIdEntidadContable AND 
       a.cIdTipoPoliza = b.cIdTipoPoliza AND
       a.nPoliza = b.nPoliza AND 
       a.cAjuste = b.cAjuste
  WHERE a.cIdDetalleCuenta = 'M' AND
        b.cIdEstadoPoliza NOT IN ('C','N') AND 
		a.cEjercicio + a.cIdEntidadContable + a.cIdTipoPoliza + CAST(a.nPoliza as CHAR(5)) + a.cAjuste + CAST(a.nMovimiento AS CHAR(4)) NOT IN
		(SELECT cEjercicio + cIdEntidadContable + cIdTipoPoliza + CAST(nPoliza as CHAR(5)) + cAjuste + CAST(nMovimiento AS CHAR(4))
		 FROM [cDetalleMovimientoPolizaCabm])
UNION ALL 
----------------------------------------------------------- RFC -----------------------------------------------
SELECT 'RFC' as Faltante, b.cIdOrigenPoliza, a.* 
  FROM [cDetalleMovimientoPoliza] a
  LEFT JOIN [cMaestroPoliza] b ON
       a.cEjercicio = b.cEjercicio AND 
       a.cIdEntidadContable = b.cIdEntidadContable AND 
       a.cIdTipoPoliza = b.cIdTipoPoliza AND
       a.nPoliza = b.nPoliza AND 
       a.cAjuste = b.cAjuste
  WHERE a.cIdDetalleCuenta = 'R' AND
        b.cIdEstadoPoliza NOT IN ('C','N') AND 
		a.cEjercicio + a.cIdEntidadContable + a.cIdTipoPoliza + CAST(a.nPoliza as CHAR(5)) + a.cAjuste + CAST(a.nMovimiento AS CHAR(4)) NOT IN
		(SELECT cEjercicio + cIdEntidadContable + cIdTipoPoliza + CAST(nPoliza as CHAR(5)) + cAjuste + CAST(nMovimiento AS CHAR(4))
		 FROM [cDetalleMovimientoPolizaRFC])
--order by 9,1,5,2,6 
) as a 
UNION ALL
----- 03 DIFERENCIAS ENTRE EL MONTO DEL MAESTRO DE POLIZA VS LA SUMA DE LOS CARGOS DE LOS MOVIMIENTOS D ELA POLIZA
SELECT '03 diferencias monto maestro vs monto movimientos' as Tipo,  COUNT(*) FROM 
(

SELECT cEjercicio, cIdEntidadContable, cIdTipoPoliza, nPoliza, cAjuste, cIdEstadoPoliza, nPeriodo, cIdOrigenPoliza, SUM(mMonto) as mMonto
  FROM 
(SELECT cEjercicio, cIdEntidadContable, cIdTipoPoliza, nPoliza, cAjuste, cIdEstadoPoliza, nPeriodo, cIdOrigenPoliza, mPoliza as mMonto  
  FROM cMaestroPoliza
 WHERE cIdEstadoPoliza IN ('R','A','O') 
UNION ALL 
SELECT a.cEjercicio, a.cIdEntidadContable, a.cIdTipoPoliza, a.nPoliza, a.cAjuste, b.cIdEstadoPoliza, b.nPeriodo, cIdOrigenPoliza, -SUM(mMovimiento) as mMonto
  FROM cDetalleMovimientoPoliza a
LEFT JOIN cMaestroPoliza b ON
		  a.cEjercicio = b.cEjercicio AND 
		  a.cIdEntidadContable = b.cIdEntidadContable AND 
		  a.cIdTipoPoliza = b.cIdTipoPoliza AND 
		  a.nPoliza = b.nPoliza AND
		  a.cAjuste = b.cAjuste
  WHERE cIdTipoMovimiento = 'C' AND
        cIdEstadoPoliza IN ('R','A','O')
  GROUP BY a.cEjercicio, a.cIdEntidadContable, a.cIdTipoPoliza, a.nPoliza, a.cAjuste, b.cIdEstadoPoliza, b.nPeriodo, b.cIdOrigenPoliza) as c   
GROUP BY cEjercicio, cIdEntidadContable, cIdTipoPoliza, nPoliza, cIdEstadoPoliza, nPeriodo, cAjuste, cIdOrigenPoliza
HAVING SUM(mMonto) <> 0   
--ORDER BY 7,3,4
) as a
UNION ALL
-- 04 DIFERENCIAS ENTRE ABONOS Y CARGOS DEL DETALLE DE MOVIMIENTOS A NIVEL DE POLIZA
SELECT '04 diferencias entre abonos y cargos movimientos' as Tipo,  COUNT(*) FROM 
(
SELECT c.nPeriodo, c.cIdOrigenPoliza, c.cIdTipoPoliza, c.cIdEstadoPoliza, c.nPoliza, sum(c.mMontoCargo) AS mCargos, sum(c.mMontoAbono) as mAbonos FROM 
(SELECT b.nPeriodo, b.cIdOrigenPoliza, a.cIdTipoPoliza, a.nPoliza, b.cIdEstadoPoliza, 
	CASE a.cIdTipoMovimiento WHEN 'C' THEN SUM(a.mMovimiento) ELSE 0000000.00 END as mMontoCargo,
	CASE a.cIdTipoMovimiento WHEN 'A' THEN SUM(a.mMovimiento) ELSE 0000000.00 END as mMontoAbono
  FROM cDetalleMovimientoPoliza a
  LEFT JOIN cMaestroPoliza b ON 
  a.cEjercicio = b.cEjercicio AND
  a.cIdEntidadContable = b.cIdEntidadContable AND
  a.cIdTipoPoliza = b.cIdTipoPoliza AND 
  a.nPoliza = b.nPoliza AND
  a.cAjuste = b.cAjuste
  WHERE cIdEstadoPoliza NOT IN ('C','N')
  GROUP BY b.nPeriodo, b.cIdOrigenPoliza, a.cIdTipoPoliza, a.nPoliza, b.cIdEstadoPoliza, a.cIdTipoMovimiento) as c
  GROUP BY nPeriodo, cIdOrigenPoliza, cIdTipoPoliza, cIdEstadoPoliza, nPoliza
  HAVING sum(c.mMontoCargo) <> sum(c.mMontoAbono)
  --ORDER BY 1,2
) as a  
UNION ALL
-- 05 DIFERENCIAS ENTRE ABONOS Y CARGOS DEL DETALLE DE MOVIMIENTOS A NIVEL DE POLIZA
SELECT '05 diferencias abonos y cargos detalle claves presupuestarias' as Tipo,  COUNT(*) FROM 
(

	SELECT c.cIdTipoPoliza, c.nPoliza, c.cIdOrigenPoliza, c.cIdEstadoPoliza, sum(c.mMontoCargo) AS mCargos, sum(c.mMontoAbono) as mAbonos FROM 
	(SELECT a.cIdTipoPoliza, a.nPoliza, d.cIdOrigenPoliza, d.cIdEstadoPoliza,
		CASE b.cIdTipoMovimiento WHEN 'C' THEN SUM(a.mMonto) ELSE 0000000.00 END as mMontoCargo,
		CASE b.cIdTipoMovimiento WHEN 'A' THEN SUM(a.mMonto) ELSE 0000000.00 END as mMontoAbono
	  FROM cDetalleMovimientoPolizaClavePresupuestariaInterna a
	  LEFT JOIN cDetalleMovimientoPoliza b ON 
					a.cEjercicio = b.cEjercicio AND
					a.cIdEntidadContable = b.cIdEntidadContable AND
					a.cIdTipoPoliza = b.cIdTipoPoliza AND
					a.nPoliza = b.nPoliza AND
					a.cAjuste = b.cAjuste AND
					a.nMovimiento = b.nMovimiento
	  LEFT JOIN cMaestroPoliza d ON 
					a.cEjercicio = d.cEjercicio AND
					a.cIdEntidadContable = d.cIdEntidadContable AND
					a.cIdTipoPoliza = d.cIdTipoPoliza AND
					a.nPoliza = d.nPoliza AND
					a.cAjuste = d.cAjuste 
	  WHERE cIdEstadoPoliza NOT IN ('C','N')								 
	  GROUP BY a.cIdTipoPoliza, a.nPoliza, b.cIdTipoMovimiento, d.cIdOrigenPoliza, d.cIdEstadoPoliza) as c
	  GROUP BY cIdTipoPoliza, nPoliza, cIdOrigenPoliza, cIdEstadoPoliza 
	  HAVING sum(c.mMontoCargo) <> sum(c.mMontoAbono)
	  --ORDER BY 1,2
  ) as a
  UNION ALL
-- 06a DIFERENCIAS ENTRE DETALLE DE MOVIMIENTOS Y DETALLE ABRM
  SELECT '06a entre detalle de movimientos y detalle RFC' as Tipo,  COUNT(*) FROM
  (
      SELECT a.* FROM cDetalleMovimientoPolizaRFC a
	  LEFT JOIN cDetalleMovimientoPoliza b ON 
		a.cIdEntidadContable = b.cIdEntidadContable AND 
		a.cIdTipoPoliza = b.cIdTipoPoliza AND 
		a.nPoliza = b.nPoliza AND 
		a.cAjuste = b.cAjuste AND 
		a.nMovimiento = b.nMovimiento
		LEFT JOIN cCatalogoCuentaContable c ON b.cIdCuentaContable = c.cIdCuentaContable
	  WHERE b.cIdCuentaContable NOT IN (SELECT cIdCuentaContable FROM cCatalogoCuentaContable WHERE lHayRFC = 1)
  ) as a
  UNION ALL
-- 06b DIFERENCIAS ENTRE DETALLE DE MOVIMIENTOS Y DETALLE BANCO
  SELECT '06b entre detalle de movimientos y detalle Banco' as Tipo,  COUNT(*) FROM cDetalleMovimientoPolizaBanco a
  LEFT JOIN cDetalleMovimientoPoliza b ON 
	a.cIdEntidadContable = b.cIdEntidadContable AND 
    a.cIdTipoPoliza = b.cIdTipoPoliza AND 
    a.nPoliza = b.nPoliza AND 
    a.cAjuste = b.cAjuste AND 
    a.nMovimiento = b.nMovimiento
    LEFT JOIN cCatalogoCuentaContable c ON b.cIdCuentaContable = c.cIdCuentaContable
  WHERE b.cIdCuentaContable NOT IN (SELECT cIdCuentaContable FROM cCatalogoCuentaContable WHERE lHayBanco = 1)
 UNION ALL
 -- 06c DIFERENCIAS ENTRE DETALLE DE MOVIMIENTOS Y DETALLE ALMACEN
  SELECT '06c entre detalle de movimientos y detalle Almacen' as Tipo,  COUNT(*) FROM cDetalleMovimientoPolizaAlmacen a
  LEFT JOIN cDetalleMovimientoPoliza b ON 
	a.cIdEntidadContable = b.cIdEntidadContable AND 
    a.cIdTipoPoliza = b.cIdTipoPoliza AND 
    a.nPoliza = b.nPoliza AND 
    a.cAjuste = b.cAjuste AND 
    a.nMovimiento = b.nMovimiento
    LEFT JOIN cCatalogoCuentaContable c ON b.cIdCuentaContable = c.cIdCuentaContable
  WHERE b.cIdCuentaContable NOT IN (SELECT cIdCuentaContable FROM cCatalogoCuentaContable WHERE lHayAlmacen = 1) 

UNION ALL
 -- 06d DIFERENCIAS ENTRE DETALLE DE MOVIMIENTOS Y DETALLE INVENTARIO
  SELECT '06d entre detalle de movimientos y detalle Inventario' as Tipo,  COUNT(*) FROM cDetalleMovimientoPolizaCabm a
	  LEFT JOIN cDetalleMovimientoPoliza b ON 
		a.cIdEntidadContable = b.cIdEntidadContable AND 
		a.cIdTipoPoliza = b.cIdTipoPoliza AND 
		a.nPoliza = b.nPoliza AND 
		a.cAjuste = b.cAjuste AND 
		a.nMovimiento = b.nMovimiento
		LEFT JOIN cCatalogoCuentaContable c ON b.cIdCuentaContable = c.cIdCuentaContable
	  WHERE b.cIdCuentaContable NOT IN (SELECT cIdCuentaContable FROM cCatalogoCuentaContable WHERE lHayCabm = 1)  

UNION ALL
  SELECT '07 lHayBanco, lHayAlmacen, lHayRFC, lHayCABM' as Tipo,  COUNT(*) FROM cCatalogoCuentaContable 
	WHERE (lHayAlmacen = 1 AND lHayBanco = 1) OR
		  (lHayAlmacen = 1 AND lHayRfc = 1) OR
		  (lHayAlmacen = 1 AND lHayCentroCosto = 1) OR
		  (lHayAlmacen = 1 AND lHayCABM = 1) OR
		  (lHayBanco = 1 AND lHayRfc = 1) OR 
		  (lHayBanco = 1 AND lHayCentroCosto = 1) OR 
		  (lHayBanco = 1 AND lHayCABM = 1) OR 
		  (lHayRFC = 1 AND lHayCentroCosto = 1) OR
		  (lHayRFC = 1 AND lHayCABM = 1) OR  
		  (lHayCentroCosto = 1 AND lHayCABM = 1)
UNION ALL 
  SELECT '08 RFC no en catalogo RFC' as Tipo,  COUNT(*) FROM cDetalleMovimientoPolizaRFC
	where cIdRFC not in (select distinct vIdRFC FROM  tCatalogoRFC)		  
	
UNION ALL
  SELECT '09 detalle de movimiento con cuentas acumulativas' as Tipo,  COUNT(*) FROM cDetalleMovimientoPoliza a
	 LEFT JOIN cCatalogoCuentaContable b ON
	 a.cIdCuentaContable = b.cIdCuentaContable
	WHERE b.cTipoCuenta <> 'D' 	
UNION ALL
-- Pueden existir polizas con suma de cargos en cero y suma de abonos en cero. Movimientos que se anulan positivos con negativos. Es valido
-- Cuando no sea el caso hay que cancelarlas
SELECT '10 suma de movtos en cargo o de abono  = 0 ' as Tipo,  COUNT(*) FROM cMaestroPoliza 

	 WHERE cIdEstadoPoliza IN ('O','A','R') AND (
	   SELECT SUM(mMovimiento) 
						  FROM cDetalleMovimientoPoliza 
						  WHERE cIdTipoMovimiento = 'C' AND
								cEjercicio = cMaestroPoliza.cEjercicio AND
								cIdEntidadContable = cMaestroPoliza.cIdEntidadContable AND
								cIdTipoPoliza = cMaestroPoliza.cIdTipoPoliza AND
								nPoliza = cMaestroPoliza.nPoliza AND 
								cAjuste = cMaestroPoliza.cAjuste
												) = 0 AND (
	   SELECT SUM(mMovimiento) 
						  FROM cDetalleMovimientoPoliza 
						  WHERE cIdTipoMovimiento = 'A' AND
								cEjercicio = cMaestroPoliza.cEjercicio AND
								cIdEntidadContable = cMaestroPoliza.cIdEntidadContable AND
								cIdTipoPoliza = cMaestroPoliza.cIdTipoPoliza AND
								nPoliza = cMaestroPoliza.nPoliza AND 
								cAjuste = cMaestroPoliza.cAjuste											
														) = 0 AND
														
	   cIdTipoPoliza + CAST(nPoliza AS CHAR(5))	NOT IN (
														SELECT cIdTipoPoliza + CAST(nPoliza AS CHAR(5))
														  FROM cDetalleMovimientoPoliza
														)  		
UNION ALL
SELECT '11 concepto movimiento en espacios' as Tipo,  COUNT(*) FROM  cDetalleMovimientoPoliza a
 
  LEFT JOIN cMaestroPoliza b ON 
  a.cIdTipoPoliza = b.cIdTipoPoliza AND 
  a.nPoliza = b.nPoliza
 WHERE cConceptoMovimiento = ''
 --ORDER BY 4,5

UNION ALL
SELECT '12 mes fecha de la poliza diferente a periodo' as Tipo,  COUNT(*) FROM  cMaestroPoliza
	WHERE MONTH(fPoliza) <> nPeriodo	
UNION ALL
SELECT '13 periodo de la poliza diferente a periodo del movimiento' as Tipo,  COUNT(*) FROM  cMaestroPoliza a
  LEFT JOIN cDetalleMovimientoPoliza b
  ON a.cEjercicio = b.cEjercicio AND
  a.cIdEntidadContable = b.cIdEntidadContable AND
  a.cIdTipoPoliza = b.cIdTipoPoliza AND
  a.nPoliza = b.nPoliza AND 
  a.cAjuste = b.cAjuste
  WHERE a.nPeriodo <> b.nPeriodo  	
UNION ALL
SELECT '14 monto detalle <> monto clave: origen contabilidad DI,IN,CH,FR' as Tipo,  COUNT(*) FROM  
  (
  SELECT b.cEjercicio, b.cIdEntidadContable, b.cIdTipoPoliza, b.nPoliza, b.cAjuste, b.nMovimiento, c.cIdCuentaContable, 
         d.nPeriodo, d.cIdOrigenPoliza, d.cIdEstadoPoliza, b.mMovimiento
  FROM 
		  (
		  SELECT cEjercicio, cIdEntidadContable, cIdTipoPoliza, nPoliza, cAjuste, nMovimiento, SUM(mMovimiento) as mMovimiento
		  FROM
				  (
				  SELECT cEjercicio, cIdEntidadContable, cIdTipoPoliza, nPoliza, cAjuste, nMovimiento, SUM(mMovimiento) as mMovimiento
				  FROM cDetalleMovimientoPoliza 
				  GROUP BY cEjercicio, cIdEntidadContable, cIdTipoPoliza, nPoliza, cAjuste, nMovimiento
				  UNION ALL 
				  SELECT cEjercicio, cIdEntidadContable, cIdTipoPoliza, nPoliza, cAjuste, nMovimiento, -SUM(mMonto) as mMovimiento
				  FROM cDetalleMovimientoPolizaClavePresupuestariaInterna
				  GROUP BY cEjercicio, cIdEntidadContable, cIdTipoPoliza, nPoliza, cAjuste, nMovimiento  
				  ) as a
		  GROUP BY cEjercicio, cIdEntidadContable, cIdTipoPoliza, nPoliza, cAjuste, nMovimiento 
		  HAVING SUM(mMovimiento) <> 0
		  ) as b
  LEFT JOIN cDetalleMovimientoPoliza c ON 
										  b.cEjercicio = c.cEjercicio AND
										  b.cIdEntidadContable = c.cIdEntidadContable AND 
										  b.cIdTipoPoliza = c.cIdTipoPoliza AND
										  b.nPoliza = c.nPoliza AND 
										  b.nMovimiento = c.nMovimiento
  LEFT JOIN cMaestroPoliza d ON 
								  b.cEjercicio = d.cEjercicio AND
								  b.cIdEntidadContable = d.cIdEntidadContable AND 
								  b.cIdTipoPoliza = d.cIdTipoPoliza AND
								  b.nPoliza = d.nPoliza 
  WHERE cIdEstadoPoliza IN  ('R','A','O') AND b.cIdTipoPoliza  IN ('DI','IN','CH','FR')
  --ORDER BY c.cIdCuentaContable, b.cIdTipoPoliza, b.nPoliza	
  ) as a
 UNION ALL
SELECT '14b monto detalle <> monto clave: No origen contabilidad' as Tipo,  COUNT(*) FROM  
  (
  SELECT b.cEjercicio, b.cIdEntidadContable, b.cIdTipoPoliza, b.nPoliza, b.cAjuste, b.nMovimiento, c.cIdCuentaContable, 
         d.nPeriodo, d.cIdOrigenPoliza, d.cIdEstadoPoliza, b.mMovimiento
  FROM 
		  (
		  SELECT cEjercicio, cIdEntidadContable, cIdTipoPoliza, nPoliza, cAjuste, nMovimiento, SUM(mMovimiento) as mMovimiento
		  FROM
				  (
				  SELECT cEjercicio, cIdEntidadContable, cIdTipoPoliza, nPoliza, cAjuste, nMovimiento, SUM(mMovimiento) as mMovimiento
				  FROM cDetalleMovimientoPoliza 
				  GROUP BY cEjercicio, cIdEntidadContable, cIdTipoPoliza, nPoliza, cAjuste, nMovimiento
				  UNION ALL 
				  SELECT cEjercicio, cIdEntidadContable, cIdTipoPoliza, nPoliza, cAjuste, nMovimiento, -SUM(mMonto) as mMovimiento
				  FROM cDetalleMovimientoPolizaClavePresupuestariaInterna
				  GROUP BY cEjercicio, cIdEntidadContable, cIdTipoPoliza, nPoliza, cAjuste, nMovimiento  
				  ) as a
		  GROUP BY cEjercicio, cIdEntidadContable, cIdTipoPoliza, nPoliza, cAjuste, nMovimiento 
		  HAVING SUM(mMovimiento) <> 0
		  ) as b
  LEFT JOIN cDetalleMovimientoPoliza c ON 
										  b.cEjercicio = c.cEjercicio AND
										  b.cIdEntidadContable = c.cIdEntidadContable AND 
										  b.cIdTipoPoliza = c.cIdTipoPoliza AND
										  b.nPoliza = c.nPoliza AND 
										  b.nMovimiento = c.nMovimiento
  LEFT JOIN cMaestroPoliza d ON 
								  b.cEjercicio = d.cEjercicio AND
								  b.cIdEntidadContable = d.cIdEntidadContable AND 
								  b.cIdTipoPoliza = d.cIdTipoPoliza AND
								  b.nPoliza = d.nPoliza 
  WHERE cIdEstadoPoliza IN  ('R','A','O') AND b.cIdTipoPoliza NOT IN ('DI','IN','CH','FR')
	
  ) as a 
 UNION ALL
 SELECT '15 monto detalle <> monto clave' as Tipo,  COUNT(*) FROM  
	(
	 SELECT cIdTipoPoliza, nPoliza, nMovimiento, SUM(mMovimiento) as mMovimiento FROM 
		(
			SELECT cIdTipoPoliza, nPoliza, nMovimiento, mMovimiento FROM cDetalleMovimientoPoliza
			  WHERE cIdTipoPoliza + CAST(nPoliza as CHAR(5)) + CAST(nMovimiento AS CHAR(5)) IN (
					SELECT cIdTipoPoliza + CAST(nPoliza as CHAR(5)) + CAST(nMovimiento AS CHAR(5)) FROM cDetalleMovimientoPolizaClavePresupuestariaInterna)
			UNION ALL
			SELECT cIdTipoPoliza, nPoliza, nMovimiento, - SUM(mMonto) as mMovimiento FROM cDetalleMovimientoPolizaClavePresupuestariaInterna
				   GROUP BY cIdTipoPoliza, nPoliza, nMovimiento
		) as a
		GROUP BY  cIdTipoPoliza, nPoliza, nMovimiento   
		HAVING SUM(mMovimiento) <> 0
	) as b
 UNION ALL
 SELECT '16 polizas sin detalle de movimientos' as Tipo,  COUNT(*) FROM  
	(
		SELECT nPoliza FROM cMaestroPoliza 
		  WHERE cIdEstadoPoliza IN  ('R','A','O') AND
		        cIdTipoPoliza + CAST(nPoliza as CHAR(5)) NOT IN (
		        SELECT cIdTipoPoliza + CAST(nPoliza as CHAR(5)) 
		          FROM cDetalleMovimientoPoliza)
	) as b	
UNION ALL
------------------------ Maestro saldos vs Detalle movimientos --------------------------

 SELECT '51 Maestro saldos vs Detalle movimientos' as Tipo,  COUNT(*) FROM  
	(
	SELECT cEjercicio, cIdEntidadContable, nPeriodo, 
		   d.cIdCuentaContable, e.cCuentaContable,
		   SUM(mCargosPeriodo) as mCargosPeriodo, 
		   SUM(mAbonosPeriodo) as mAbonosPeriodo 
	FROM 	   
	(SELECT  cEjercicio, cIdEntidadContable, nPeriodo, cIdCuentaContable,
			mCargosPeriodo,
			mAbonosPeriodo 
	  FROM cMaestroSaldoCuenta
	  WHERE nPeriodo > 0
	UNION ALL 
	SELECT a.cEjercicio, a.cIdEntidadContable, a.nPeriodo, b.cIdCuentaContable,  
		-SUM(case b.cIdTipoMovimiento when 'C' THEN b.mMovimiento ELSE 0 END) AS mCargosPeriodo,
		-SUM(case b.cIdTipoMovimiento when 'A' THEN b.mMovimiento ELSE 0 END) AS mAbonosPeriodo 
		FROM cMaestroPoliza a, cDetalleMovimientoPoliza b
		WHERE a.cEjercicio = b.cEjercicio AND 
		a.cIdEntidadContable = b.cIdEntidadContable AND 
		a.cIdTipoPoliza = b.cIdTipoPoliza AND 
		a.nPoliza = b.nPoliza AND 
		a.cAjuste = b.cAjuste AND 
		a.cIdEstadoPoliza = 'O'
		GROUP BY a.cEjercicio, a.cIdEntidadContable, a.nPeriodo, b.cIdCuentaContable) AS d
		LEFT JOIN cCatalogoCuentaContable e ON d.cIdCuentaContable = e.cIdCuentaContable
		GROUP BY cEjercicio, cIdEntidadContable, nPeriodo, d.cIdCuentaContable, e.cCuentaContable 
		HAVING SUM(mCargosPeriodo) <> 0 OR SUM(mAbonosPeriodo) <> 0	
) as a		
UNION ALL
----- diferencias entre detalle saldo cuentas almacen vs detalle movimiento polizas almacen
 SELECT '52 detalle saldo cuentas almacen vs detalle movimiento polizas almacen' as Tipo,  COUNT(*) FROM  
	(
SELECT cEjercicio, cIdEntidadContable, nPeriodo, 
       d.cIdCuentaContable, e.cCuentaContable,
       d.vIdAlmacen, f.vAlmacen, 
	   SUM(mCargosPeriodo) as mCargosPeriodo, 
	   SUM(mAbonosPeriodo) as mAbonosPeriodo 
FROM 	   
(SELECT  cEjercicio, cIdEntidadContable, nPeriodo, cIdCuentaContable, vIdAlmacen, 
		mCargosPeriodo,
		mAbonosPeriodo 
  FROM cDetalleSaldoCuentaAlmacen
  WHERE nPeriodo > 0
UNION ALL 
SELECT a.cEjercicio, a.cIdEntidadContable, a.nPeriodo, b.cIdCuentaContable, c.vIdAlmacen, 
    -SUM(case b.cIdTipoMovimiento when 'C' THEN b.mMovimiento ELSE 0 END) AS mCargosPeriodo,
    -SUM(case b.cIdTipoMovimiento when 'A' THEN b.mMovimiento ELSE 0 END) AS mAbonosPeriodo 
    FROM cMaestroPoliza a, cDetalleMovimientoPoliza b, 
    cDetalleMovimientoPolizaAlmacen c
    WHERE a.cEjercicio = b.cEjercicio AND 
    a.cIdEntidadContable = b.cIdEntidadContable AND 
    a.cIdTipoPoliza = b.cIdTipoPoliza AND 
    a.nPoliza = b.nPoliza AND 
    a.cAjuste = b.cAjuste AND 
    b.cEjercicio = c.cEjercicio AND 
    b.cIdEntidadContable = c.cIdEntidadContable AND 
    b.cIdTipoPoliza = c.cIdTipoPoliza AND 
    b.nPoliza = c.nPoliza AND 
    b.cAjuste = c.cAjuste AND 
    b.nMovimiento = c.nMovimiento AND 
    a.cIdEstadoPoliza = 'O'
    GROUP BY a.cEjercicio, a.cIdEntidadContable, a.nPeriodo, b.cIdCuentaContable, c.vIdAlmacen) AS d
    LEFT JOIN cCatalogoCuentaContable e ON d.cIdCuentaContable = e.cIdCuentaContable
    LEFT JOIN uCatalogoAlmacen f ON d.vIdAlmacen = f.vIdAlmacen
    GROUP BY cEjercicio, cIdEntidadContable, nPeriodo, d.cIdCuentaContable, d.vIdAlmacen, e.cCuentaContable, f.vAlmacen
    HAVING SUM(mCargosPeriodo) <> 0 OR SUM(mAbonosPeriodo) <> 0
) as a	
UNION ALL
----- diferencias entre detalla saldo cuentas banco vs detalle movimiento polizas banco 
	SELECT '53 diferencias entre detalla saldo cuentas banco vs detalle movimiento polizas banco' as Tipo,  COUNT(*) FROM  
	(

	SELECT cEjercicio, cIdEntidadContable, nPeriodo, 
		   d.cIdCuentaContable, e.cCuentaContable,
		   d.nConsecutivo, f.vCuenta AS cBanco, 
		   SUM(mCargosPeriodo) as mCargosPeriodo, 
		   SUM(mAbonosPeriodo) as mAbonosPeriodo 
	FROM 	   
	(SELECT  cEjercicio, cIdEntidadContable, nPeriodo, cIdCuentaContable, nConsecutivo, 
			mCargosPeriodo,
			mAbonosPeriodo 
	  FROM cDetalleSaldoCuentaBanco
	  WHERE nPeriodo > 0
	UNION ALL 
	SELECT a.cEjercicio, a.cIdEntidadContable, a.nPeriodo, b.cIdCuentaContable, c.nConsecutivo, 
		-SUM(case b.cIdTipoMovimiento when 'C' THEN b.mMovimiento ELSE 0 END) AS mCargosPeriodo,
		-SUM(case b.cIdTipoMovimiento when 'A' THEN b.mMovimiento ELSE 0 END) AS mAbonosPeriodo 
		FROM cMaestroPoliza a, cDetalleMovimientoPoliza b, 
		cDetalleMovimientoPolizaBanco c
		WHERE a.cEjercicio = b.cEjercicio AND 
		a.cIdEntidadContable = b.cIdEntidadContable AND 
		a.cIdTipoPoliza = b.cIdTipoPoliza AND 
		a.nPoliza = b.nPoliza AND 
		a.cAjuste = b.cAjuste AND 
		b.cEjercicio = c.cEjercicio AND 
		b.cIdEntidadContable = c.cIdEntidadContable AND 
		b.cIdTipoPoliza = c.cIdTipoPoliza AND 
		b.nPoliza = c.nPoliza AND 
		b.cAjuste = c.cAjuste AND 
		b.nMovimiento = c.nMovimiento AND 
		a.cIdEstadoPoliza = 'O'
		GROUP BY a.cEjercicio, a.cIdEntidadContable, a.nPeriodo, b.cIdCuentaContable, c.nConsecutivo) AS d
		LEFT JOIN cCatalogoCuentaContable e ON d.cIdCuentaContable = e.cIdCuentaContable
		LEFT JOIN tCuentaBancariaInstitucion f ON d.nConsecutivo = f.nConsecutivo
		GROUP BY cEjercicio, cIdEntidadContable, nPeriodo, d.cIdCuentaContable, d.nConsecutivo, e.cCuentaContable, f.vCuenta
		HAVING SUM(mCargosPeriodo) <> 0 OR SUM(mAbonosPeriodo) <> 0
) as a
UNION ALL
----- diferencias entre detalle saldo cuentas RFC vs detalle movimiento polizas RFC
SELECT '54 diferencias entre detalle saldo cuentas RFC vs detalle movimiento polizas RFC' as Tipo,  COUNT(*) FROM  
	(
		SELECT cEjercicio, cIdEntidadContable, nPeriodo, 
			   d.cIdCuentaContable, e.cCuentaContable,
			   d.cIdRFC, f.vRazonSocial AS cRFC, 
			   SUM(mCargosPeriodo) as mCargosPeriodo, 
			   SUM(mAbonosPeriodo) as mAbonosPeriodo 
		FROM 	   
		(SELECT  cEjercicio, cIdEntidadContable, nPeriodo, cIdCuentaContable, cIdRFC, 
				mCargosPeriodo,
				mAbonosPeriodo 
		  FROM cDetalleSaldoCuentaRFC
		  WHERE nPeriodo > 0
		UNION ALL 
		SELECT a.cEjercicio, a.cIdEntidadContable, a.nPeriodo, b.cIdCuentaContable, c.cIdRFC, 
			-SUM(case b.cIdTipoMovimiento when 'C' THEN b.mMovimiento ELSE 0 END) AS mCargosPeriodo,
			-SUM(case b.cIdTipoMovimiento when 'A' THEN b.mMovimiento ELSE 0 END) AS mAbonosPeriodo 
			FROM cMaestroPoliza a, cDetalleMovimientoPoliza b, cDetalleMovimientoPolizaRFC c
			WHERE a.cEjercicio = b.cEjercicio AND 
			a.cIdEntidadContable = b.cIdEntidadContable AND 
			a.cIdTipoPoliza = b.cIdTipoPoliza AND 
			a.nPoliza = b.nPoliza AND 
			a.cAjuste = b.cAjuste AND 
			b.cEjercicio = c.cEjercicio AND 
			b.cIdEntidadContable = c.cIdEntidadContable AND 
			b.cIdTipoPoliza = c.cIdTipoPoliza AND 
			b.nPoliza = c.nPoliza AND 
			b.cAjuste = c.cAjuste AND 
			b.nMovimiento = c.nMovimiento AND 
			a.cIdEstadoPoliza = 'O'
			GROUP BY a.cEjercicio, a.cIdEntidadContable, a.nPeriodo, b.cIdCuentaContable, c.cIdRFC) AS d
			LEFT JOIN cCatalogoCuentaContable e ON d.cIdCuentaContable = e.cIdCuentaContable
			LEFT JOIN tCatalogoRFC f ON d.cIdRFC = f.vIdRFC
			GROUP BY cEjercicio, cIdEntidadContable, nPeriodo, d.cIdCuentaContable, d.cIdRFC, e.cCuentaContable, f.vRazonSocial
			HAVING SUM(mCargosPeriodo) <> 0 OR SUM(mAbonosPeriodo) <> 0
	) as a
UNION ALL 
 
 --- Los negativos están de más en el detalle de saldos. Los positivos están de más en maestro saldos-------------------------------------------
--SELECT * FROM cCatalogoCuentaContable where cIdCuentaContable in ('012410000100002     ','012410000200002     ','012410000300002     ','012410000900002     ','012420000100002     ','012420000300002     ','012420000900002     ','012460000500002     ','012460000600002     ','012460000700002     ','012470000100002     ') order by 1
--SELECT * FROM cCatalogoCuentaContable order by 1
SELECT '55 Los negativos están de más en el detalle de saldos. Los positivos están de más en maestro saldos' as Tipo,  COUNT(*) FROM  
	(


	SELECT cEjercicio, cIdEntidadContable, nPeriodo, cIdCuentaContable,
		   SUM(mAbonos) AS mAbonos, SUM(mCargos) AS mCargos
	  FROM 	   
	(SELECT cEjercicio, cIdEntidadContable, nPeriodo, cIdCuentaContable,
		   SUM(mAbonosPeriodo) AS mAbonos, SUM(mCargosPeriodo) AS mCargos
	  FROM cMaestroSaldoCuenta
	  --WHERE cIdCuentaContable = '0111200001' AND nPeriodo = 2
	  GROUP BY cEjercicio, cIdEntidadContable, nPeriodo, cIdCuentaContable 
	UNION ALL  
	SELECT cEjercicio, cIdEntidadContable, nPeriodo, cIdCuentaContable,
			-SUM(mAbonosPeriodo) as mAbonos, -SUM(mCargosPeriodo) as mCargos
		FROM cDetalleSaldoCuentaBanco
		--WHERE cIdCuentaContable = '0111200001' AND nPeriodo = 2
		GROUP BY cEjercicio, cIdEntidadContable, nPeriodo, cIdCuentaContable 
	 UNION ALL
	SELECT cEjercicio, cIdEntidadContable, nPeriodo, cIdCuentaContable,
			-SUM(mAbonosPeriodo) as mAbonos, -SUM(mCargosPeriodo) as mCargos
		FROM cDetalleSaldoCuentaAlmacen
		--WHERE cIdCuentaContable = '0111200001' AND nPeriodo = 2
		GROUP BY cEjercicio, cIdEntidadContable, nPeriodo, cIdCuentaContable  
		
	 UNION ALL
	SELECT cEjercicio, cIdEntidadContable, nPeriodo, cIdCuentaContable,
			-SUM(mAbonosPeriodo) as mAbonos, -SUM(mCargosPeriodo) as mCargos
		FROM cDetalleSaldoCuentaCABM
		--WHERE cIdCuentaContable = '0111200001' AND nPeriodo = 2
		GROUP BY cEjercicio, cIdEntidadContable, nPeriodo, cIdCuentaContable  	
	UNION ALL
	SELECT cEjercicio, cIdEntidadContable, nPeriodo, cIdCuentaContable,
			-SUM(mAbonosPeriodo) as mAbonos, -SUM(mCargosPeriodo) as mCargos
		FROM cDetalleSaldoCuentaRFC
		--WHERE cIdCuentaContable = '0111200001' AND nPeriodo = 2
		GROUP BY cEjercicio, cIdEntidadContable, nPeriodo, cIdCuentaContable  
	UNION ALL
	SELECT cEjercicio, cIdEntidadContable, nPeriodo, cIdCuentaContable,
			-SUM(mAbonosPeriodo) as mAbonos, -SUM(mCargosPeriodo) as mCargos
		FROM cDetalleSaldoCuentaSinDetalle
		--WHERE cIdCuentaContable = '0111200001' AND nPeriodo = 2
		GROUP BY cEjercicio, cIdEntidadContable, nPeriodo, cIdCuentaContable) as b  
	GROUP BY cEjercicio, cIdEntidadContable, nPeriodo, cIdCuentaContable	
	HAVING (SUM(mAbonos)  <> 0 OR SUM(mCargos) <> 0) AND (SUM(mAbonos)  <>  SUM(mCargos))   
) as a	

UNION ALL
----- diferencias entre detalle saldo cuentas CABM vs detalle movimiento polizas CABM
SELECT '56 diferencias entre detalle saldo cuentas CABM vs detalle movimiento polizas CABM' as Tipo,  COUNT(*) FROM  
	(

	SELECT cEjercicio, cIdEntidadContable, nPeriodo, 
		   d.cIdCuentaContable, e.cCuentaContable,
		   d.nIdCABM, f.cIdCABM, f.vCABM, 
		   SUM(mCargosPeriodo) as mCargosPeriodo, 
		   SUM(mAbonosPeriodo) as mAbonosPeriodo 
	FROM 	   
	(SELECT  cEjercicio, cIdEntidadContable, nPeriodo, cIdCuentaContable, nIdCABM, 
			mCargosPeriodo,
			mAbonosPeriodo 
	  FROM cDetalleSaldoCuentaCABM
	  WHERE nPeriodo > 0
	UNION ALL 
	SELECT a.cEjercicio, a.cIdEntidadContable, a.nPeriodo, b.cIdCuentaContable, c.nIdCABM, 
		-SUM(case b.cIdTipoMovimiento when 'C' THEN b.mMovimiento ELSE 0 END) AS mCargosPeriodo,
		-SUM(case b.cIdTipoMovimiento when 'A' THEN b.mMovimiento ELSE 0 END) AS mAbonosPeriodo 
		FROM cMaestroPoliza a, cDetalleMovimientoPoliza b, cDetalleMovimientoPolizaCABM c
		WHERE a.cEjercicio = b.cEjercicio AND 
		a.cIdEntidadContable = b.cIdEntidadContable AND 
		a.cIdTipoPoliza = b.cIdTipoPoliza AND 
		a.nPoliza = b.nPoliza AND 
		a.cAjuste = b.cAjuste AND 
		b.cEjercicio = c.cEjercicio AND 
		b.cIdEntidadContable = c.cIdEntidadContable AND 
		b.cIdTipoPoliza = c.cIdTipoPoliza AND 
		b.nPoliza = c.nPoliza AND 
		b.cAjuste = c.cAjuste AND 
		b.nMovimiento = c.nMovimiento AND 
		a.cIdEstadoPoliza = 'O'
		GROUP BY a.cEjercicio, a.cIdEntidadContable, a.nPeriodo, b.cIdCuentaContable, c.nIdCABM) AS d
		LEFT JOIN cCatalogoCuentaContable e ON d.cIdCuentaContable = e.cIdCuentaContable
		LEFT JOIN cCatalogoCABM f ON d.nIdCABM = f.nIdCABM
		GROUP BY cEjercicio, cIdEntidadContable, nPeriodo, d.cIdCuentaContable, d.nIdCABM, e.cCuentaContable, f.cIdCABM, f.vCABM
		HAVING SUM(mCargosPeriodo) <> 0 OR SUM(mAbonosPeriodo) <> 0
) as a


SELECT '1. Modificado' as Tabla, * FROM eClavePresupuestariaInternaModificado 
 where mEnero < 0 OR mFebrero  < 0 OR mMarzo  < 0 OR mAbril  < 0 OR 
       mMayo < 0 OR mJunio < 0 OR mJulio  < 0 OR mAgosto  < 0 OR
       mSeptiembre  < 0 OR mOctubre  < 0 OR mNoviembre  < 0 OR mDiciembre < 0 
UNION ALL
SELECT '2. Precomprometido' as Tabla, * FROM eClavePresupuestariaInternaPreComprometido 
 where mEnero < 0 OR mFebrero  < 0 OR mMarzo  < 0 OR mAbril  < 0 OR 
       mMayo < 0 OR mJunio < 0 OR mJulio  < 0 OR mAgosto  < 0 OR
       mSeptiembre  < 0 OR mOctubre  < 0 OR mNoviembre  < 0 OR mDiciembre < 0 
UNION ALL 
SELECT '3. Comprometido' as Tabla, * FROM eClavePresupuestariaInternaComprometido
 where mEnero < 0 OR mFebrero  < 0 OR mMarzo  < 0 OR mAbril  < 0 OR 
       mMayo < 0 OR mJunio < 0 OR mJulio  < 0 OR mAgosto  < 0 OR
       mSeptiembre  < 0 OR mOctubre  < 0 OR mNoviembre  < 0 OR mDiciembre < 0 
UNION ALL
SELECT '4. Devengado' as Tabla, * FROM eClavePresupuestariaInternaDevengado
 where mEnero < 0 OR mFebrero  < 0 OR mMarzo  < 0 OR mAbril  < 0 OR 
       mMayo < 0 OR mJunio < 0 OR mJulio  < 0 OR mAgosto  < 0 OR
       mSeptiembre  < 0 OR mOctubre  < 0 OR mNoviembre  < 0 OR mDiciembre < 0 
UNION ALL
SELECT '5. Ejercido' as Tabla, * FROM eClavePresupuestariaInternaEjercido 
 where mEnero < 0 OR mFebrero  < 0 OR mMarzo  < 0 OR mAbril  < 0 OR 
       mMayo < 0 OR mJunio < 0 OR mJulio  < 0 OR mAgosto  < 0 OR
       mSeptiembre  < 0 OR mOctubre  < 0 OR mNoviembre  < 0 OR mDiciembre < 0 
UNION ALL
SELECT '6. Pagado' as Tabla, * FROM eClavePresupuestariaInternaPagado 
 where mEnero < 0 OR mFebrero  < 0 OR mMarzo  < 0 OR mAbril  < 0 OR 
       mMayo < 0 OR mJunio < 0 OR mJulio  < 0 OR mAgosto  < 0 OR
       mSeptiembre  < 0 OR mOctubre  < 0 OR mNoviembre  < 0 OR mDiciembre < 0 
ORDER BY nIdClaveIngresos, nIdClaveEgresos   
